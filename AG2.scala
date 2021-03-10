import scala.annotation.tailrec
import scala.util.NotGiven

// Represent the programs environment
case class Env(
  val line: Int = 0,                                // The current line the interpreter should be on, used for WithIRef
  val stack_ops: List[List[Symbol]] = List(),       // The stack, as the original source operations instead of the data
  val cs: List[Int] = List(),                       // The call stack, containing the return addresses as line numbers
  val cmp_uses: List[Symbol] = List(),              // The operations that formed the internal comparator via 'cmp
  val op_sources: Map[String, Symbol] = Map(),      // Associations from registers to the operation that created them
  val dyn_loc_sources: List[Symbol] = List(),       // The dynamic operations that lead to the current location of line
  val mov_sources: Map[String, String] = Map(),     // Associations from registers to their original source
  val linked: Set[Tuple2[Symbol, Symbol]] = Set(),  // Which operations are dependent on each other, as known so far
  val cond_fork_depth: Int = 0                      // Length of the current chain of conditional jumps that made this
):
  def accessOp(reg: String): Option[Symbol] =
    op_sources.get( mov_sources.get(reg).getOrElse(reg) )
  end accessOp

trait SteppedRepr[T]:
  def step(env: T): T

given SteppedRepr[Env] with
  def step(env: Env): Env = Env(
    env.line + 1,
    env.stack_ops, env.cs, env.cmp_uses, env.op_sources,
    env.dyn_loc_sources, env.mov_sources, env.linked, env.cond_fork_depth
  )

given [T : SteppedRepr]: SteppedRepr[Traversable[T]] with
  def step(env: Traversable[T]): Traversable[T] =
    env.map(summon[SteppedRepr[T]].step)
  end step

trait WithIRef[-T, -E, +R]:
  def retrieve(env: E, expr: T): R

object ParallelExprs:
  opaque type ParallelExpr[+T] = List[T]
  
  object ParallelExpr:
    def apply[T](tr: Traversable[T]): ParallelExpr[T] = tr.toList

  extension [T](x : ParallelExpr[T])
    def toList: List[T] = x

import ParallelExprs._

given [T]: WithIRef[Traversable[T], Env, T] with
  def retrieve(env: Env, expr: Traversable[T]): T =
    expr.drop(env.line).head
  end retrieve

given [T, E, R](
  using iref: WithIRef[Traversable[T], E, R]
): WithIRef[Traversable[T], Traversable[E], ParallelExpr[R]] with
  def retrieve(env: Traversable[E], expr: Traversable[T]): ParallelExpr[R] =
    ParallelExpr( env.map {
      iref.retrieve(_, expr)
    } )
  end retrieve

trait CheckedHyperEnv[E] extends HyperI[E]:
  abstract override def shift(env: E): Traversable[E] =
    val result = super.shift(env)
    return result.flatMap { x => if (
      summon[VerifiableEnv[E]].verify(result)
    ) then Some(x) else None }
  end shift

trait VerifiableEnv[-E]:
  def verify(env: E): Boolean

given VerifiableEnv[Env] with
  def verify(env: Env): Boolean = env.cond_fork_depth <= 10

given [T](using NotGiven[VerifiableEnv[T]]): VerifiableEnv[T] with
  def verify(env: T): Boolean = true

type ResEnv[+V, +E] = Tuple2[Option[V], E]

trait Interpretable[-T, E, +V]:
  def eval(env: E, expr: T)(using stepper: SteppedRepr[E]): ResEnv[V, E] =
    None -> stepper.step( this.exec(env, expr) )
  end eval
  def exec(env: E, expr: T)(using stepper: SteppedRepr[E]): E =
    stepper.step( this.eval(env, expr)._2 )
  end exec

trait HyperI[E]:
  def shift(env: E): Traversable[E]

// A normal operation, like 'dec
case class StandardOp(op: Symbol, gen: List[String], uses: List[String])          extends HyperI[Env]:
  def shift(env: Env): Traversable[Env] = Some(Env(
    env.line, env.stack_ops, env.cs, env.cmp_uses,
    env.op_sources ++ Map( gen.map { _ -> op } : _*),
    env.dyn_loc_sources, env.mov_sources,
    env.linked ++ Map( uses.flatMap(env.accessOp).map {
      _ -> op
    } : _*) ++ Map( env.dyn_loc_sources.map {
      _ -> op
    } : _*), env.cond_fork_depth
  ))

// An instruction that pushes something onto the stack, like 'push
case class StackPush(src: String)                                                 extends HyperI[Env]:
  def shift(env: Env): Traversable[Env] = Some(Env(
    env.line, (
      env.accessOp(src).toList ::: env.dyn_loc_sources
    ) :: env.stack_ops, env.cs, env.cmp_uses, env.op_sources,
    env.dyn_loc_sources, env.mov_sources, env.linked, env.cond_fork_depth
  ))

// An instruction that pops something off of the stack, like 'pop
case class StackPop(target: String)                                               extends HyperI[Env]:
  def shift(env: Env): Traversable[Env] = env.stack_ops match
    case local_sources :: remaining => Some(Env(
      env.line, remaining, env.cs, env.cmp_uses,
      env.op_sources ++ Map( local_sources.map {
        target -> _
      } : _* ), env.dyn_loc_sources, env.mov_sources,
      env.linked, env.cond_fork_depth
    ))
    case Nil => None

// An instruction that triggers the internal comparator, like 'cmp
case class ComparatorTrigger(lhs: String, rhs: String)                            extends HyperI[Env]:
  def shift(env: Env): Traversable[Env] = Some(Env(
    env.line, env.stack_ops, env.cs, List(
      env.accessOp(lhs), env.accessOp(rhs)
    ).flatten ::: env.dyn_loc_sources, env.op_sources,
    env.dyn_loc_sources, env.mov_sources, env.linked, env.cond_fork_depth
  ))

// A mov-like instruction, like 'mov or 'xchg
case class TransferOp(op: Symbol, target: String, src: String)                    extends HyperI[Env]:
  def shift(env: Env): Traversable[Env] =
    val pure_transfer = (op == Symbol("mov")).||(
      op == Symbol("lea") || op == Symbol("xchg")
    )
    val new_link_info =
      if pure_transfer then
        (env.op_sources, env.linked)
      else
        (
          env.op_sources + (target -> op),
          env.linked ++ Map(( env.accessOp(src).map {
            _ -> op
          }).toList : _* ) ++ Map( env.dyn_loc_sources.map {
            _ -> op
          } : _* )
        )
    val new_mov_sources =
      if op == Symbol("xchg") then
        env.mov_sources ++ Map(
          target -> env.mov_sources.get(src).getOrElse(src),
          src -> env.mov_sources.get(target).getOrElse(target)
        )
      else
        env.mov_sources + ( target -> (
          env.mov_sources.get(src).getOrElse(src)
        ) )
    return Some(Env(
      env.line, env.stack_ops, env.cs, env.cmp_uses,
      new_link_info._1, env.dyn_loc_sources, new_mov_sources,
      new_link_info._2, env.cond_fork_depth
    ))
  end shift

// A no-op, like 'nop or 'hint_nop7
case class NoOp()                                                                 extends HyperI[Env]:
  def shift(env: Env): Traversable[Env] = Some(env)

// An unconditional jump, like 'jmp
case class UnconditionalJump(target: Int)                                         extends HyperI[Env]:
  def shift(env: Env): Traversable[Env] = Some(Env(
    target, env.stack_ops, env.cs, env.cmp_uses, env.op_sources,
    env.dyn_loc_sources, env.mov_sources, env.linked, env.cond_fork_depth
  ))

// A conditional jump, like 'jnz, but without verification
case class RawConditionalJump(op: Symbol, target: Int)                            extends HyperI[Env]:
  def shift(env: Env): Traversable[Env] =
    List(env.line, target) map {
      Env(
        _, env.stack_ops, env.cs, env.cmp_uses, env.op_sources,
        op :: env.dyn_loc_sources, env.mov_sources, env.linked,
        env.cond_fork_depth + 1
      )
    }
  end shift

// A conditional jump, like 'jnz, but with verification
class ConditionalJump(override val op: Symbol, override val target: Int)
  extends RawConditionalJump(op, target) with CheckedHyperEnv[Env]

// A call to a procedure, like 'call
case class ProcedureCall(target: Int)                                             extends HyperI[Env]:
  def shift(env: Env): Traversable[Env] =
    val return_addr = env.line + 1
    return Some(Env(
      target,
      env.stack_ops,
      return_addr :: env.cs,
      env.cmp_uses, env.op_sources, env.dyn_loc_sources,
      env.mov_sources, env.linked, env.cond_fork_depth
    ))
  end shift

// A return from a procedures, like 'ret
case class ProcedureReturn()                                                      extends HyperI[Env]:
  def shift(env: Env): Traversable[Env] = env.cs match
    case return_addr :: cs_remaining
      => Some(Env(
        return_addr, env.stack_ops, cs_remaining,
        env.cmp_uses, env.op_sources, env.dyn_loc_sources,
        env.mov_sources, env.linked, env.cond_fork_depth
      ))
    case Nil => None

given [E]: Interpretable[ParallelExpr[HyperI[E]], Traversable[E], Nothing] with
  override def exec(env: Traversable[E], expr: ParallelExpr[HyperI[E]])(
    using stepper: SteppedRepr[Traversable[E]]
  ): Traversable[E] = stepper.step( expr.toList.zip(env).flatMap {
    _.shift(_)
  } )

// T := HyperI[Env]
// E := Traversable[Env]
// iref : WithIRef[Traversable[T], E, Y]
// iref : WithIRef[Traversable[T], Traversable[Env], ParallelExpr[R]]
//        E := Env
//        Y := ParallelExpr[R]
//        using iref : WithIRef[Traversable[T], E, R]
//        using iref : WithIRef[Traversable[T], E, T]
//        R := T
// Y := ParallelExpr[T]
// interpreter : Interpretable[Y, E, Nothing]
// interpreter : Interpretable[ParallelExpr[HyperI[Env]], Traversable[Env], Nothing]

given [E : SteppedRepr, T, Y](
  using iref: WithIRef[Traversable[T], E, Y]
)(
  using interpreter: Interpretable[Y, E, Nothing]
): Interpretable[Traversable[T], E, Nothing] with
  override def exec(env: E, expr: Traversable[T])(
    using SteppedRepr[E]
  ) = execRecursive(env, expr)

  @tailrec
  final def execRecursive(env: E, expr: Traversable[T])(
    using SteppedRepr[E]
  ): E = if expr.nonEmpty
    then execRecursive(interpreter.exec(
      env,
      iref.retrieve(env, expr)
    ), expr)
  else env

/*
given [E : SteppedRepr, T](
  using NotGiven[Interpretable[T, E, T]]
): Interpretable[T, E, T] with
  override def eval(env: E, expr: T)(using stepper: SteppedRepr[E]) =
    Some(expr) -> stepper.step(env)
  end eval
*/

@main def main = println(test())

def test()(
  using interpreter: Interpretable[Traversable[HyperI[Env]], Traversable[Env], Nothing]
) /* : Env */ = 90
