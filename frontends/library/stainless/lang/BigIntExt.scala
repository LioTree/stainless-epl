package stainless.lang

import stainless.annotation.library
import stainless.collection.List

import scala.language.implicitConversions

@library
case class BigIntExt(val underlying: BigInt) {
  def +(that: BigIntExt): BigIntExt = BigIntExt(underlying + that.underlying)

  def +(that: StringExt): StringExt = toStringExt + that

  def +(that: Rational): Rational = Rational(underlying) + that

  def -(that: BigIntExt): BigIntExt = BigIntExt(underlying - that.underlying)

  def -(that: Rational): Rational = Rational(underlying) - that

  def *(that: BigIntExt): BigIntExt = BigIntExt(underlying * that.underlying)

  def *(that: Rational): Rational = Rational(underlying) * that

  def /(that: BigIntExt): BigIntExt = BigIntExt(underlying / that.underlying)

  def /(that: Rational): Rational = Rational(underlying) / that

  def %(that: BigIntExt): BigIntExt = BigIntExt(underlying % that.underlying)

  def <(that: BigIntExt): Boolean = underlying < that.underlying

  def >(that: BigIntExt): Boolean = underlying > that.underlying

  def <=(that: BigIntExt): Boolean = underlying <= that.underlying

  def >=(that: BigIntExt): Boolean = underlying >= that.underlying

  def !=(that: BigIntExt): Boolean = underlying != that.underlying

  def toStringExt: StringExt = {
    if (underlying == 0) return "0"

    var number = underlying
    val isNegative = number < 0
    if (isNegative) number = -number
    var digits = List[String]()

    while (number > 0) {
      decreases(number)
      val digit = number % 10
      val digitStr = digit match {
        case 0 => "0"
        case 1 => "1"
        case 2 => "2"
        case 3 => "3"
        case 4 => "4"
        case 5 => "5"
        case 6 => "6"
        case 7 => "7"
        case 8 => "8"
        case 9 => "9"
      }
      digits = digitStr :: digits
      number /= 10
    }

    if (isNegative) digits = "-" :: digits

    StringExt(List.mkString(digits, "", x => x))
  }
}

@library
object BigIntExt {
  def apply(num: BigInt): BigIntExt = new BigIntExt(num)

  def unapply(bigIntExt: BigIntExt): Option[BigInt] = Some(bigIntExt.underlying)

  implicit def bigIntToBigIntExt(b: BigInt): BigIntExt = BigIntExt(b)

  implicit def bigIntExtToBigInt(b: BigIntExt): BigInt = b.underlying
}