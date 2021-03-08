import scala.language.implicitConversions

trait GEnv[+T] {
  def empty: T
}

given GEnv[Int] with
  def empty = 0

case class OptTargetEnv[+E : GEnv, +V](env: E, value: Option[V])

given [E : GEnv, V]: Conversion[OptTargetEnv[E, V], E] = _.env

given [E : GEnv]: Conversion[E, OptTargetEnv[E, Nothing]] = OptTargetEnv(_, None)

def main(args: Array[String]) =
  val z: OptTargetEnv[Int, Nothing] = OptTargetEnv.Without(Env(7))
  val xprocess: Env => Int = a => a.x
  println(((a: GEnv) => xprocess(a) + 1)(z))
end main

