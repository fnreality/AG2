import scala.util.NotGiven

// Represent these programs environments
class Env(
  val line: Int,                            // The current line the program is on, akin to the instruction pointer
  val stack_refs: List[String],             // The stack, as named references to the original sources, (moves handled)
  val cs: List[Int],                        // The call stack, containing the return addresses as line numbers
  val cmp_uses: Tuple2[Symbol, Symbol],     // The current targets of the internal comparator, manipulated with 'cmp
  val op_sources: Map[String, Symbol],      // Associations from registers to the operation that created them
  val dyn_loc_sources: List[Symbol],        // The dynamic operations that lead to the current location of line
  val mov_sources: Map[String, String],     // Associations from registers to their original source their data is from
  val linked: Set[Tuple2[Symbol, Symbol]]   // Which operations are dependent on each other, their part of the result
)

trait Interpretable[T, E, V]:
  def eval(expr: T, env: E): Tuple2[V, E]

class StandardOp(op: Symbol, gen: [String], uses: [String])     // A normal operation, like 'dec
class NoOp                                                      // A no-op, 'nop or 'hint_nop7


given Interpretable[StandardOp, Env, Unit] with
  def eval(expr: StandardOp, env: Env) =
    ???
  end eval

given Interpretable[NoOp, Env, Unit] with
  def eval(expr: NoOp, env: Env) =
    () -> env
  end eval

given [T](using NotGiven[Interpretable[T, Unit, T]]) with
  def eval(expr: T, env: Unit) =
    expr -> ()
  end eval
