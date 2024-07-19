object Test1 {
  import scala.math._

  val a = abs(1)
}

object Test2 {
  import scala.math

  val a = math.abs(1)
}

object Test3 {
  val a = math.abs(1)
}

object Test4 {
  val b = scala.math.abs(1)
}