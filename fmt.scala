import scala.util.NotGiven

type ResEnv[+V, +E] = Tuple2[Option[V], E]

trait Interpretable[-T, E, +V] {
  def eval(env: E, expr: T): ResEnv[V, E] =
    None -> this.exec(env, expr)
  end eval
  def exec(env: E, expr: T): E =
    this.eval(env, expr)._2
  end exec
}

trait Instruction[E] {
  def step(env: E): E
}

class TestOp extends Instruction[Int]:
  def step(env: Int) =
    env + 1
  end step

given [E, T <: Instruction[E]]: Interpretable[T, E, Nothing] with
  override def exec(env: E, expr: T) =
    expr step env
  end exec

given [E, T](using NotGiven[Interpretable[T, E, T]]): Interpretable[T, E, T] with
  override def eval(env: E, expr: T) =
    Some(expr) -> env
  end eval
