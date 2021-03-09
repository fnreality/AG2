import scala.util.NotGiven

// Represent the programs environment
case class Env(
  val line: Int,                            // The current line the program is on, akin to the instruction pointer
  val stack_ops: List[List[Symbol]],        // The stack, as the original source operations of what was referenced
  val cs: List[Int],                        // The call stack, containing the return addresses as line numbers
  val cmp_uses: List[Symbol],               // The used operations of the internal comparator, manipulated with 'cmp
  val op_sources: Map[String, Symbol],      // Associations from registers to the operation that created them
  val dyn_loc_sources: List[Symbol],        // The dynamic operations that lead to the current location of line
  val mov_sources: Map[String, String],     // Associations from registers to their original source their data is from
  val linked: Set[Tuple2[Symbol, Symbol]],  // Which operations are dependent on each other, their part of the result
  val cond_fork_depth: Int                  // How long the current chain of conditional jumps that this represents is
):
  def accessOp(reg: String): Option[Symbol] =
    op_sources get ( mov_sources get reg getOrElse reg )
  end accessOp

trait SteppedRepr[T]:
  def step(env: T): T

given SteppedRepr[Env] with
  def step(env: Env): Env = Env(
    env.line + 1,
    env.stack_ops, env.cs, env.cmp_uses, env.op_sources,
    env.dyn_loc_sources, env.mov_sources, env.linked, env.cond_fork_depth
  )

given [T](using NotGiven[SteppedRepr[T]]): SteppedRepr[T] with
  def step(env: T): T = env

trait CheckEnv[E]:
  def exec(env: E): Traversable[E] =
    val result = super exec env
    return result flatMap { x => if (
      summon[VerifiableEnv[E]] verify result
    ) then Some(x) else None }
  end exec

trait VerifiableEnv[-E]:
  def verify(env: E): Boolean

given VerifiableEnv[Env] with
  def verify(env: E): Boolean = env.cond_fork_depth <= 10

given [T](using NotGiven[VerifiableEnv[T]]): VerifiableEnv[T] with
  def verify(env: E): Boolean = true

type ResEnv[+V, +E] = Tuple2[Option[V], E]

trait Interpretable[-T, E : SteppedRepr, +V]:
  def eval(env: E, expr: T)(using stepper: SteppedRepr[E]): ResEnv[V, E] =
    None -> ( stepper step this.exec(env, expr) )
  end eval
  def exec(env: E, expr: T)(using stepper: SteppedRepr[E]): E =
    stepper step this.eval(env, expr)._2
  end exec

trait HyperInstr[E]:
  def shift(env: E): Traversable[E]

// A normal operation, like 'dec
case class StandardOp(op: Symbol, gen: List[String], uses: List[String])      extends HyperInstr[Env] with CheckEnv[Env]:
  def shift(env: Env): Traversable[Env] = Some(Env(
    env.line, env.stack_ops, env.cs, env.cmp_uses,
    env.op_sources ++ Map( gen map { _ -> op } : _*),
    env.dyn_loc_sources, env.mov_sources,
    env.linked ++ Map( uses flatMap env.accessOp map {
      _ -> op
    } : _*) ++ Map( env.dyn_loc_sources map {
      _ -> op
    } : _*), env.cond_fork_depth
  ))

// An instruction that pushes something onto the stack, like 'push
case class StackPush(src: String)                                             extends HyperInstr[Env] with CheckEnv[Env]:
  def shift(env: Env): Traversable[Env] = Some(Env(
    env.line, (
      ( env accessOp src ).toList ::: env.dyn_loc_sources
    ) :: env.stack_ops, env.cs, env.cmp_uses, env.op_sources,
    env.dyn_loc_sources, env.mov_sources, env.linked, env.cond_fork_depth
  ))

// An instruction that pops something off of the stack, like 'pop
case class StackPop(target: String)                                           extends HyperInstr[Env] with CheckEnv[Env]:
  def shift(env: Env): Traversable[Env] = env.stack_ops match
    case local_sources :: remaining => Some(Env(
      env.line, remaining, env.cs, env.cmp_uses,
      env.op_sources ++ Map( local_sources map {
        target -> _
      } : _* ), env.dyn_loc_sources, env.mov_sources,
      env.linked, env.cond_fork_depth
    ))
    case Nil => None

// An instruction that triggers the internal comparator, like 'cmp
case class ComparatorTrigger(lhs: String, rhs: String)                        extends HyperInstr[Env] with CheckEnv[Env]:
  def shift(env: Env): Traversable[Env] = Some(Env(
    env.line, env.stack_ops, env.cs, List(
      env accessOp lhs, env accessOp rhs
    ).flatten ::: env.dyn_loc_sources, env.op_sources,
    env.dyn_loc_sources, env.mov_sources, env.linked, env.cond_fork_depth
  ))

// A mov-like instruction, like 'mov or 'xchg
case class TransferOp(op: Symbol, target: String, src: String)                extends HyperInstr[Env] with CheckEnv[Env]:
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
          env.linked ++ Map(( env accessOp src map {
            _ -> op
          }).toList : _* ) ++ Map( env.dyn_loc_sources map {
            _ -> op
          } : _* )
        )
    val new_mov_sources =
      if op == Symbol("xchg") then
        env.mov_sources ++ Map(
          target -> (env.mov_sources get src getOrElse src),
          src -> (env.mov_sources get target getOrElse target)
        )
      else
        env.mov_sources + ( target -> (
          env.mov_sources get src getOrElse src
        ) )
    return Some(Env(
      env.line, env.stack_ops, env.cs, env.cmp_uses,
      new_link_info._1, env.dyn_loc_sources, new_mov_sources,
      new_link_info._2, env.cond_fork_depth
    ))
  end shift

// A no-op, like 'nop or 'hint_nop7
case class NoOp()                                                             extends HyperInstr[Env] with CheckEnv[Env]:
  def shift(env: Env): Traversable[Env] = Some(env)

// An unconditional jump, like 'jmp
case class UnconditionalJump(target: Int)                                     extends HyperInstr[Env] with CheckEnv[Env]:
  def shift(env: Env): Traversable[Env] = Some(Env(
    target, env.stack_ops, env.cs, env.cmp_uses, env.op_sources,
    env.dyn_loc_sources, env.mov_sources, env.linked, env.cond_fork_depth
  ))

// A conditional jump, like 'jnz
case class ConditionalJump(op: Symbol, target: Int)                           extends HyperInstr[Env] with CheckEnv[Env]:
  def shift(env: Env): Traversable[Env] = List(env.line, target) map {
    Env(
      _, env.stack_ops, env.cs, env.cmp_uses, env.op_sources,
      op :: env.dyn_loc_sources, env.mov_sources, env.linked,
      env.cond_fork_depth + 1
    )
  }

// A call to a procedure, like 'call
case class ProcedureCall(target: Int)                                         extends HyperInstr[Env] with CheckEnv[Env]:
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
case class ProcedureReturn()                                                  extends HyperInstr[Env] with CheckEnv[Env]:
  def shift(env: Env): Traversable[Env] = env.cs match
    case return_addr :: cs_remaining
      => Some(Env(
        return_addr, env.stack_ops, cs_remaining,
        env.cmp_uses, env.op_sources, env.dyn_loc_sources,
        env.mov_sources, env.linked, env.cond_fork_depth
      ))
    case Nil => None

given [E, T <: Instruction[E]]: Interpretable[T, E, Nothing] with
  override def exec(env: E, expr: T)(using stepper: SteppedRepr[E]) =
    stepper step ( expr shift env )
  end exec

given [E, T <: HyperInstr[E]]: Intepretable[T, Traversable[E], Nothing] with
  override def exec(env: Traversable[E], expr: T)(
    using stepper: SteppedRepr[Traversable[E]]
  ) = env flatMap expr.exec

given [E, T](
  using interpreter: Interpretable[T, E, Nothing]
): Interpretable[Traversable[T], E, Nothing] with
  override def exec(env: E, expr: Traversable[T])(
    using SteppedRepr[E]
  ) = ( expr foldLeft env )(interpreter.exec)

given [E, T](using NotGiven[Interpretable[T, E, T]]): Interpretable[T, E, T] with
  override def eval(env: E, expr: T)(using stepper: SteppedRepr[E]) =
    Some(expr) -> ( stepper step env )
  end eval
