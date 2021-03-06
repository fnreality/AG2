import scala.language.postfixOps

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

// A trait representing something that can be executed
sealed trait Interpreted {
  def eval(env: Env): Env // An instruction needs to be able to be evaluated
}

// A trait representing an instruction that can be executed
sealed trait Instruction extends Interpreted

// A normal operation, like 'dec
case class StandardOp(op: Symbol, gen: List[String], uses: List[String])      extends Instruction {
  def eval(env: Env): Env = {
    ???
  }
}

// A no-op, like 'nop or 'hint_nop7
case class NoOp()                                                             extends Instruction {
  def eval(env: Env): Env = env                                               // Do nothing (the identity function)
}

// An unconditional jump, like 'jmp
case class UnconditionalJump(target: Int)                                     extends Instruction {
  def eval(env: Env): Env = Env(
    target,                                                                   // Overwrite the instruction pointer
    env.stack_refs, env.cs, env.cmp_uses,                                     // Preserve everything else
    env.op_sources, env.dyn_loc_sources, env.mov_sources,                     // Including our interpretation of it
    env.linked                                                                // Including the intermediate result
  )
}

// A conditional jump, like 'jnz
case class ConditionalJump(op: Symbol, target: Int)                           extends Instruction {
  def eval(env: Env): Env = {
    ???
  }
}

// A call to a procedure, like 'call
case class ProcedureCall(target: Int)                                         extends Instruction {
  def eval(env: Env): Env = {
    val return_addr = env.line + 1                                            // Calculate the return address
    return Env(
      target,                                                                 // Overwrite the instruction pointer
      env.stack_refs,                                                         // Preserve the stack
      return_addr :: env.cs,                                                  // But update the virtual call stack
      env.cmp_uses, env.op_sources, env.dyn_loc_sources, env.mov_sources,     // Preserve everything else
      env.linked                                                              // Including the intermediate result
    )
  }
}

// A return from a procedures, like 'ret
case class ProcedureReturn()                                                  extends Instruction {
  def eval(env: Env): Env = env.cs match {
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
}

// A list of instructions, a program that is abstracted over its environment
case class DependentProgram(instructions: Traversable[Instruction])           extends Interpreted {
  def eval(env: Env): Env = {
    ???
  }
}
