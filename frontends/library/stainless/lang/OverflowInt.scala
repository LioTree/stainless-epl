package stainless.lang

import stainless.annotation.library
import stainless.collection.List
import stainless.annotation._
import stainless.lang.OverflowInt.overflow

import scala.language.implicitConversions

/*
Although Stainless can use backend solvers to simulate Int overflow rules with --strict-arithmetic=false,
some APIs in the Stainless library are only available in the BigInt version (such as bigSubstring for String).
The two types are not interchangeable except for Int literals.
Therefore, we have introduced an OverflowInt class that uses BigInt to simulate Int overflow rules.
*/

@library
case class OverflowInt(underlying: BigInt) {

  require(underlying >= OverflowInt.IntMin && underlying <= OverflowInt.IntMax, "OverflowInt out of bounds")

  def +(that: OverflowInt): OverflowInt = OverflowInt.overflow(underlying + that.underlying)

  def +(that: StringWrapper): StringWrapper = StringWrapper(toString) + that

  def -(that: OverflowInt): OverflowInt = OverflowInt.overflow(underlying - that.underlying)

  def *(that: OverflowInt): OverflowInt = OverflowInt.overflow(underlying * that.underlying)

  def /(that: OverflowInt): OverflowInt = OverflowInt.overflow(underlying / that.underlying)

  def %(that: OverflowInt): OverflowInt = OverflowInt.overflow(underlying % that.underlying)

  def <(that: OverflowInt): Boolean = underlying < that.underlying

  def >(that: OverflowInt): Boolean = underlying > that.underlying

  def <=(that: OverflowInt): Boolean = underlying <= that.underlying

  def >=(that: OverflowInt): Boolean = underlying >= that.underlying

  def !=(that: OverflowInt): Boolean = underlying != that.underlying

  @extern
  @pure
  def ^(that: OverflowInt): OverflowInt = OverflowInt(0)

  def abs: OverflowInt = OverflowInt(stainless.math.abs(underlying))

  override def toString: String = {
    if (underlying == 0) return "0"

    var number = underlying
    val isNegative = number < 0
    if (isNegative) number = -number
    var digits = List[String]()

    while (number > 0) {
      decreases(number)
      val digit = number % 10
      // There will be some verification error if using match pattern. I don't know why.
      val digitStr = {
        if (digit == 0) "0"
        else if (digit == 1) "1"
        else if (digit == 2) "2"
        else if (digit == 3) "3"
        else if (digit == 4) "4"
        else if (digit == 5) "5"
        else if (digit == 6) "6"
        else if (digit == 7) "7"
        else if (digit == 8) "8"
        else "9"
      }
      digits = digitStr :: digits
      number /= 10
    }

    if (isNegative) digits = "-" :: digits
    List.mkString(digits, "", x => x)
  }
}

@library
object OverflowInt {
  private val IntMax: BigInt = Int.MaxValue
  private val IntMin: BigInt = Int.MinValue

  private def overflow(num: BigInt): OverflowInt = {
    if (num > IntMax) OverflowInt(num - IntMax - 1 + IntMin)
    else if (num < IntMin) OverflowInt(num - IntMin + 1 + IntMax)
    else OverflowInt(num)
  } ensuring (res => res.underlying >= IntMin && res.underlying <= IntMax)

  def apply(num: BigInt): OverflowInt = {
    require(num >= IntMin && num <= IntMax, "OverflowInt out of bounds")
    new OverflowInt(num)
  }

  def unapply(overflowInt: OverflowInt): Option[BigInt] = Some(overflowInt.underlying)

  implicit def bigInt2OverflowInt(b: BigInt): OverflowInt = OverflowInt(b)

  implicit def overflowInt2BigInt(b: OverflowInt): BigInt = {
    b.underlying
  } ensuring(res => res >= IntMin && res <= IntMax)
}