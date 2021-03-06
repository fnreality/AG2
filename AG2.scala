import scala.annotation.tailrec
import scala.util.NotGiven

type Tr[+T] = Traversable[T]

// Represent the programs environment
case class CPUHyperEnv(
  val line: Int = 0,                                // The current line the interpreter should be on, used for WithIRef
  val stack_ops: List[List[String]] = List(),       // The stack, as the original source operations instead of the data
  val cs: List[Int] = List(),                       // The call stack, containing the return addresses as line numbers
  val cmp_uses: List[String] = List(),              // The operations that formed the internal comparator via 'cmp
  val op_sources: Map[String, String] = Map(),      // Associations from registers to the operation that created them
  val dyn_loc_sources: List[String] = List(),       // The dynamic operations that lead to the current location of line
  val mov_sources: Map[String, String] = Map(),     // Associations from registers to their original source
  val linked: Set[Tuple2[String, String]] = Set(),  // Which operations are dependent on each other, as known so far
  val cond_fork_depth: Int = 0                      // Length of the current chain of conditional jumps that made this
):
  def accessOp(reg: String): Option[String] =
    op_sources.get( mov_sources.get(reg).getOrElse(reg) )
  end accessOp

trait WithNoOp[+T]:
  def noOp: T

trait SteppedRepr[T]:
  def step(env: T): T

given SteppedRepr[CPUHyperEnv] with
  def step(env: CPUHyperEnv): CPUHyperEnv = CPUHyperEnv(
    env.line + 1,
    env.stack_ops, env.cs, env.cmp_uses, env.op_sources,
    env.dyn_loc_sources, env.mov_sources, env.linked, env.cond_fork_depth
  )

given [T : SteppedRepr]: SteppedRepr[Tr[T]] with
  def step(env: Tr[T]): Tr[T] =
    env.map(summon[SteppedRepr[T]].step)
  end step

trait WithIRef[-T, -E, +R]:
  def retrieve(env: E, expr: T): R
  def hasRemaining(env: E, expr: T): Boolean

object ParallelExprs:
  opaque type ParallelExpr[+T] = List[T]
  
  object ParallelExpr:
    def apply[T](tr: Tr[T]): ParallelExpr[T] = tr.toList

  extension [T](x : ParallelExpr[T])
    def toList: List[T] = x

import ParallelExprs._

given [T : WithNoOp]: WithIRef[Tr[T], CPUHyperEnv, T] with
  def retrieve(env: CPUHyperEnv, expr: Tr[T]): T =
    if hasRemaining(env, expr)
      then expr.drop(env.line).head
    else summon[WithNoOp[T]].noOp
  end retrieve
  
  def hasRemaining(env: CPUHyperEnv, expr: Tr[T]): Boolean =
    expr.size > env.line
  end hasRemaining

given [T, E, R](
  using iref: WithIRef[Tr[T], E, R]
): WithIRef[Tr[T], Tr[E], ParallelExpr[R]] with
  def retrieve(env: Tr[E], expr: Tr[T]): ParallelExpr[R] =
    ParallelExpr( env.map {
      iref.retrieve(_, expr)
    } )
  end retrieve
  
  def hasRemaining(env: Tr[E], expr: Tr[T]): Boolean =
    !env.exists(!iref.hasRemaining(_, expr))
  end hasRemaining

trait CheckedHyperEnv[E] extends HyperI[E]:
  abstract override def shift(env: E): Tr[E] =
    val result = super.shift(env)
    return result.flatMap { x => if (
      summon[VerifiableEnv[E]].verify(result)
    ) then Some(x) else None }
  end shift

trait VerifiableEnv[-E]:
  def verify(env: E): Boolean

given VerifiableEnv[CPUHyperEnv] with
  def verify(env: CPUHyperEnv): Boolean =
    env.cond_fork_depth <= 10
  end verify

given [T](using NotGiven[VerifiableEnv[T]]): VerifiableEnv[T] with
  def verify(env: T): Boolean = true

type ResCPUEnv[+V, +E] = Tuple2[Option[V], E]

trait Interpretable[-T, E, +V]:
  def eval(env: E, expr: T)(using stepper: SteppedRepr[E]): ResCPUEnv[V, E] =
    None -> stepper.step( this.exec(env, expr) )
  end eval
  def exec(env: E, expr: T)(using stepper: SteppedRepr[E]): E =
    stepper.step( this.eval(env, expr)._2 )
  end exec

trait HyperI[E]:
  def shift(env: E): Tr[E]

type CPUHyperI = HyperI[CPUHyperEnv]

given WithNoOp[CPUHyperI] with
  def noOp: CPUHyperI = NoOp()

// A normal operation, like 'dec
case class StandardOp(op: String, gen: List[String], uses: List[String])          extends CPUHyperI:
  def shift(env: CPUHyperEnv): Tr[CPUHyperEnv] = Some(env.copy(
    op_sources= env.op_sources ++ Map( gen.map { _ -> op } : _*),
    linked= env.linked ++ Map( (gen ++ uses).flatMap(env.accessOp).map {
      _ -> op
    } : _*) ++ Map( env.dyn_loc_sources.map {
      _ -> op
    } : _*)
  ))

// An instruction that pushes something onto the stack, like 'push
case class StackPush(src: String)                                                 extends CPUHyperI:
  def shift(env: CPUHyperEnv): Tr[CPUHyperEnv] = Some(env.copy(stack_ops= (
      env.accessOp(src).toList ::: env.dyn_loc_sources
    ) :: env.stack_ops))

// An instruction that pops something off of the stack, like 'pop
case class StackPop(target: String)                                               extends CPUHyperI:
  def shift(env: CPUHyperEnv): Tr[CPUHyperEnv] = env.stack_ops match
    case local_sources :: remaining
      => Some(env.copy(
        stack_ops= remaining,
        op_sources= env.op_sources ++ Map( local_sources.map {
          target -> _
        } : _* )
      ))
    case Nil => None

// An instruction that triggers the internal comparator, like 'cmp
case class ComparatorTrigger(lhs: String, rhs: String)                            extends CPUHyperI:
  def shift(env: CPUHyperEnv): Tr[CPUHyperEnv] = Some(env.copy(
    cmp_uses= List(
      env.accessOp(lhs), env.accessOp(rhs)
    ).flatten ::: env.dyn_loc_sources
  ))

// A mov-like instruction, like 'mov or 'xchg
case class TransferOp(op: String, target: String, src: String)                    extends CPUHyperI:
  def shift(env: CPUHyperEnv): Tr[CPUHyperEnv] =
    val pure_transfer = (op == "mov").||(
      op == "lea" || op == "xchg"
    )
    // new_link_info ::= (op_sources, linked)
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
      if op == "xchg" then
        env.mov_sources ++ Map(
          target -> env.mov_sources.get(src).getOrElse(src),
          src -> env.mov_sources.get(target).getOrElse(target)
        )
      else
        env.mov_sources + ( target -> (
          env.mov_sources.get(src).getOrElse(src)
        ) )
    return Some(env.copy(
      op_sources= new_link_info._1, linked= new_link_info._2
    ))
  end shift

// A no-op, like 'nop or 'hint_nop7
case class NoOp()                                                                 extends CPUHyperI:
  def shift(env: CPUHyperEnv): Tr[CPUHyperEnv] = Some(env)

// An unconditional jump, like 'jmp, but without verification
case class RawUnconditionalJump(target: Int)                                      extends CPUHyperI:
  def shift(env: CPUHyperEnv): Tr[CPUHyperEnv] = Some(env.copy(line= target - 1))

// An unconditional jump, like 'jmp, but with verification
class UnconditionalJump(override val target: Int)
  extends RawUnconditionalJump(target) with CheckedHyperEnv[CPUHyperEnv]

// A conditional jump, like 'jnz, but without verification
case class RawConditionalJump(op: String, target: Int)                            extends CPUHyperI:
  def shift(env: CPUHyperEnv): Tr[CPUHyperEnv] =
    List(env.line, target - 1) map {
      local_target => env.copy(
        line= local_target,
        dyn_loc_sources= op :: (env.dyn_loc_sources ++ env.cmp_uses),
        linked= env.linked ++ env.cmp_uses.map( _ -> op ),
        cond_fork_depth= env.cond_fork_depth + 1
      )
    }
  end shift

// A conditional jump, like 'jnz, but with verification
class ConditionalJump(override val op: String, override val target: Int)
  extends RawConditionalJump(op, target) with CheckedHyperEnv[CPUHyperEnv]

// A call to a procedure, like 'call, but without verification
case class RawProcedureCall(target: Int)                                          extends CPUHyperI:
  def shift(env: CPUHyperEnv): Tr[CPUHyperEnv] =
    val return_addr = env.line + 1
    return Some(env.copy(line= target, cs= return_addr :: env.cs))
  end shift

// A call to a procedure, like 'call, but with verification
class ProcedureCall(override val target: Int)
  extends RawProcedureCall(target) with CheckedHyperEnv[CPUHyperEnv]

// A return from a procedures, like 'ret
case class ProcedureReturn()                                                      extends CPUHyperI:
  def shift(env: CPUHyperEnv): Tr[CPUHyperEnv] = env.cs match
    case return_addr :: cs_remaining
      => Some(env.copy(line= return_addr, cs= cs_remaining))
    case Nil => None

given [E]: Interpretable[ParallelExpr[HyperI[E]], Tr[E], Nothing] with
  override def exec(env: Tr[E], expr: ParallelExpr[HyperI[E]])(
    using stepper: SteppedRepr[Tr[E]]
  ): Tr[E] = stepper.step( expr.toList.zip(env).flatMap {
    _.shift(_)
  } )

// T := CPUHyperI
// E := Tr[CPUHyperEnv]
// iref : WithIRef[Tr[T], E, Y]
// iref : WithIRef[Tr[T], Tr[CPUHyperEnv], ParallelExpr[R]]
//        E := CPUHyperEnv
//        Y := ParallelExpr[R]
//        using iref : WithIRef[Tr[T], E, R]
//        using iref : WithIRef[Tr[T], E, T]
//        R := T
// Y := ParallelExpr[T]
// interpreter : Interpretable[Y, E, Nothing]
// interpreter : Interpretable[ParallelExpr[CPUHyperI], Tr[CPUHyperEnv], Nothing]

given [E : SteppedRepr, T, Y](
  using iref: WithIRef[Tr[T], E, Y]
)(
  using interpreter: Interpretable[Y, E, Nothing]
): Interpretable[Tr[T], E, Nothing] with
  override def exec(env: E, expr: Tr[T])(
    using SteppedRepr[E]
  ) = execRecursive(env, expr)

  @tailrec
  final def execRecursive(env: E, expr: Tr[T])(
    using SteppedRepr[E]
  ): E = if iref.hasRemaining(env, expr)
    then execRecursive(interpreter.exec(
      env,
      iref.retrieve(env, expr)
    ), expr)
  else env

given [E : SteppedRepr, T](
  using NotGiven[Interpretable[T, E, T]]
): Interpretable[T, E, T] with
  override def eval(env: E, expr: T)(using stepper: SteppedRepr[E]) =
    Some(expr) -> stepper.step(env)
  end eval

type CPUHyperInterpreter = Interpretable[Tr[CPUHyperI], Tr[CPUHyperEnv], Nothing]

def graph_repr(instructions: Tr[CPUHyperI])(
  using interpreter: CPUHyperInterpreter
): Set[Tuple2[String, String]] =
  interpreter.exec(List(new CPUHyperEnv()), instructions)
    .map(_.linked)
    .map(_.filter(_ != _))
    .reduce(_ ++ _)

def main(args: Array[String]) = 
  assert(graph_repr(List(
    NoOp(),
    StandardOp("dec", List("eax"), List()),
    StandardOp("inc", List("ebx"), List()),
    TransferOp("xchg", "eax", "ebx"),
    StandardOp("add", List("ecx"), List("eax")),
    ComparatorTrigger("ecx", "ebx"),
    ConditionalJump("jnz", 10),
    UnconditionalJump(0),
    StandardOp("add", List("[0xA]"), List("edx")),
    ProcedureReturn(),
    StandardOp("xlatb", List("eax"), List("ebx")),
    StandardOp("dec", List("edx"), List()),
    StackPush("edx"),
    StackPop("[0xA]"),
    StandardOp("sub", List("eax"), List("[0xA]")),
    ProcedureCall(8)
  )) == Set(
      ("dec" -> "xlatb"), ("inc" -> "xlatb"), ("inc" -> "dec"), ("add" -> "xlatb"), ("dec" -> "add"),
      ("jnz" -> "dec"), ("inc" -> "sub"), ("add" -> "jnz"), ("xlatb" -> "sub"), ("inc" -> "jnz"),
      ("add" -> "inc"), ("jnz" -> "inc"), ("add" -> "sub"), ("jnz" -> "sub"), ("jnz" -> "add"),
      ("add" -> "dec"), ("inc" -> "add"), ("jnz" -> "xlatb")
  ))
  println("ok")
end main

