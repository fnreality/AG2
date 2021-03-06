import scala.language.postfixOps

// Represent the programs environment
case class Env(
    val line: Int,                            // The current line the program is on, akin to the instruction pointer
    val stack_refs: List[String],             // The stack, as named references to the original sources, (moves handled)
    val cs: List[Int],                        // The call stack, containing the return addresses as line numbers
    val cmp_state: ComparatorState,           // The current state of the internal comparator, manipulated with 'cmp
    val op_sources: Map[String, Symbol],      // Associations from registers to the operation that created them
    val dyn_loc_sources: List[Symbol],        // The dynamic operations that lead to the current location of line
    val mov_sources: Map[String, String],     // Associations from registers to their original source their data is from
    val linked: Set[Tuple2[Symbol, Symbol]]   // Which operations are dependent on each other, their part of the result
)

// A trait representing an instruction that can be executed in this context
trait Instruction {
  def eval(env: Env): Env // An instruction needs to be able to be evaluated
}

// A normal operation, like 'dec
case class StandardOp(op: Symbol, gen: List[String], uses: List[String])    extends Instruction {
  def eval(env: Env): Env = {
    ???
  }
}

// A no-op, like 'nop or 'hint_nop7
case class NoOp()                                                           extends Instruction {
  def eval(env: Env): Env = env
}

// An unconditional jump, like 'jmp
case class UnconditionalJump(target: Int)                                   extends Instruction {
  def eval(env: Env): Env = Env(
    target,
    env.stack_refs, env.cs, env.cmp_state, env.op_sources, env.dyn_loc_sources, env.mov_sources,
    env.linked
  )
}

// A conditional jump, like 'jnz
case class ConditionalJump(op: Symbol, target: Int)                         extends Instruction {
  def eval(env: Env): Env = Env
}

