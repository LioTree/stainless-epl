import stainless.lang.*
import stainless.annotation.*

object OverflowIntTest {
  def overflow(): OverflowInt = {
    OverflowInt(2147483647) + OverflowInt(1)
  } ensuring (res => res.underlying == -2147483648)

  def underflow(): OverflowInt = {
    OverflowInt(-2147483648) - OverflowInt(1)
  } ensuring (res => res.underlying == 2147483647)

  def overflowEqual1(n: OverflowInt): OverflowInt = {
    n + OverflowInt(1)
  }

  def overflowEqual2(n: OverflowInt): OverflowInt = {
    n + OverflowInt(2) - OverflowInt(1)
  } ensuring(_ == overflowEqual1(n))

  def toStringTest(): String = {
    val a = OverflowInt(2147483647)
    a.toString
  } ensuring(_ == "2147483647")

  def toStringTest2(): String = {
    val a = OverflowInt(-2147483648)
    a.toString
  } ensuring (_ == "-2147483648")}