import scala.util.NotGiven

// Represent the programs environment
case class Env(
  val line: Int,                            // The current line the program is on, akin to the instruction pointer
  val stack_refs: List[String],             // The stack, as named references to the original sources, (moves handled)
  val cs: List[Int],                        // The call stack, containing the return addresses as line numbers
  val cmp_uses: Tuple2[Symbol, Symbol],     // The current targets of the internal comparator, manipulated with 'cmp
  val op_sources: Map[String, Symbol],      // Associations from registers to the operation that created them
  val dyn_loc_sources: List[Symbol],        // The dynamic operations that lead to the current location of line
  val mov_sources: Map[String, String],     // Associations from registers to their original source their data is from
  val linked: Set[Tuple2[Symbol, Symbol]]   // Which operations are dependent on each other, their part of the result
)

trait SteppedRepr[T]:
  def step(env: T): T

given SteppedRepr[Env] with
  def step(env: Env): Env = Env(
    env.line + 1,
    env.stack_refs, env.cs, env.cmp_uses, env.op_sources,
    env.dyn_loc_sources, env.mov_sources, env.linked
  )

given [T](using NotGiven[SteppedRepr[T]]): SteppedRepr[T] with
  def step(env: T): T = env

type ResEnv[+V, +E] = Tuple2[Option[V], E]

trait Interpretable[-T, E : SteppedRepr, +V]:
  def eval(env: E, expr: T)(using stepper: SteppedRepr[E]): ResEnv[V, E] =
    None -> ( stepper step this.exec(env, expr) )
  end eval
  def exec(env: E, expr: T)(using stepper: SteppedRepr[E]): E =
    stepper step this.eval(env, expr)._2
  end exec

trait Instruction[E]:
  def shift(env: E): E

// A normal operation, like 'dec
case class StandardOp(op: Symbol, gen: List[String], uses: List[String])      extends Instruction[Env]:
  def shift(env: Env): Env = Env(
    env.line, env.stack_refs, env.cs, env.cmp_uses,
    env.op_sources ++ Map( gen map { _ -> op } : _*),
    env.dyn_loc_sources, env.mov_sources,
    env.linked ++ Map( uses flatMap {
      env.op_sources get _
    } map { _ -> op } : _*)
  )

enum StackOpDirection {
  case Push, Pop
}

// An instruction that pushes something onto the stack, like 'push
case class StackPush(src: String)                                             extends Instruction[Env]:
  def shift(env: Env): Env = ???

// An instruction that pops something off of the stack, like 'pop
case class StackPop(target: String)                                           extends Instruction[Env]:
  def shift(env: Env): Env = ???

// An instruction that triggers the internal comparator, like 'cmp
case class ComparatorTrigger(lhs: String, rhs: String)                        extends Instruction[Env]:
  def shift(env: Env): Env = ???

// A mov-like instruction, like 'mov or 'xchg
case class TransferOp(op: Symbol, target: String, src: String)                extends Instruction[Env]:
  def shift(env: Env): Env =
    val pure_transfer = (op == Symbol("mov")).||(
      op == Symbol("lea") || op == Symbol("xchg")
    )
    val new_link_info =
      if pure_transfer then
        (env.op_sources, env.linked)
      else
        (
          env.op_sources + (target -> op),
          env.linked ++ Map( env.op_sources get src flatMap {
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
    Env(
      env.line, env.stack_refs, env.cs, env.cmp_uses, new_link_info._1,
      env.dyn_loc_sources, new_mov_sources, new_link_info._2
    )
  end shift

// A no-op, like 'nop or 'hint_nop7
case class NoOp()                                                             extends Instruction[Env]:
  def shift(env: Env): Env = env                                               // Do nothing (the identity function)

// An unconditional jump, like 'jmp
case class UnconditionalJump(target: Int)                                     extends Instruction[Env]:
  def shift(env: Env): Env = Env(
    target,                                                                   // Overwrite the instruction pointer
    env.stack_refs, env.cs, env.cmp_uses,                                     // Preserve everything else
    env.op_sources, env.dyn_loc_sources, env.mov_sources, env.linked          // Including our interpretation of it
  )

// A conditional jump, like 'jnz
case class ConditionalJump(op: Symbol, target: Int)                           extends Instruction[Env]:
  def shift(env: Env): Env = ???

// A call to a procedure, like 'call
case class ProcedureCall(target: Int)                                         extends Instruction[Env]:
  def shift(env: Env): Env =
    val return_addr = env.line + 1                                            // Calculate the return address
    return Env(
      target,                                                                 // Overwrite the instruction pointer
      env.stack_refs,                                                         // Preserve the stack
      return_addr :: env.cs,                                                  // But update the virtual call stack
      env.cmp_uses, env.op_sources, env.dyn_loc_sources, env.mov_sources,     // Preserve everything else
      env.linked                                                              // Including the intermediate result
    )
  end shift

// A return from a procedures, like 'ret
case class ProcedureReturn()                                                  extends Instruction[Env]:
  def shift(env: Env): Env = env.cs match {
    case return_addr :: cs_remaining                                          // If the call stack has a return address
      => Env(
        return_addr,                                                          // Jump to the return address
        env.stack_refs,                                                       // Preserve the stack
        cs_remaining,                                                         // Return the remains of the call stack
        env.cmp_uses, env.op_sources, env.dyn_loc_sources, env.mov_sources,   // Preserve everything else
        env.linked                                                            // Including the intermediate result
      )
    case _                                                                    // Otherwise, if the call stack is empty
      => throw new IllegalArgumentException("Call Stack Underflow")           // Error out, this is not allowed
  }

given [E, T <: Instruction[E]]: Interpretable[T, E, Nothing] with
  override def exec(env: E, expr: T) =
    expr shift env
  end exec

given[E, T](using Interpretable[T, E, Nothing]): Interpretable[Traversable[T], E, Nothing] with
  override def exec(env: E, expr: Traversable[T])(using stepper: SteppedRepr[E])(
    using interpreter: Interpretable[T, E, Nothing]
  ) = ( expr foldLeft env )(Function.untupled(
    interpreter.exec.tupled andThen stepper.step
  ))

given [E, T](using NotGiven[Interpretable[T, E, T]]): Interpretable[T, E, T] with
  override def eval(env: E, expr: T)(using stepper: SteppedRepr[E]) =
    Some(expr) -> ( stepper step env )
  end eval

