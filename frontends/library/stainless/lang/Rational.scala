/* Copyright 2009-2021 EPFL, Lausanne */

package stainless.lang

import stainless.annotation._
import stainless.lang.StaticChecks._

import scala.language.implicitConversions

@library
case class Rational(numerator: BigInt, denominator: BigInt) {

  require(denominator > 0)

  def +(that: Rational): Rational = {
    Rational(this.numerator * that.denominator + that.numerator * this.denominator, this.denominator * that.denominator)
  }

  def -(that: Rational): Rational = {
    Rational(this.numerator * that.denominator - that.numerator * this.denominator, this.denominator * that.denominator)
  }

  def unary_- : Rational = {
    Rational(-this.numerator, this.denominator)
  }

  def *(that: Rational): Rational = {
    Rational(this.numerator * that.numerator, this.denominator * that.denominator)
  }

  def /(that: Rational): Rational = {
    require(that.nonZero)
    val newNumerator = this.numerator * that.denominator
    val newDenominator = this.denominator * that.numerator
    normalize(newNumerator, newDenominator)
  }

  def +(that: BigIntExt): Rational = {
    val thatRational: Rational = that
    this + thatRational
  }

  def -(that: BigIntExt): Rational = {
    val thatRational: Rational = that
    this - thatRational
  }

  def *(that: BigIntExt): Rational = {
    val thatRational: Rational = that
    this * thatRational
  }

  def /(that: BigIntExt): Rational = {
    val thatRational: Rational = that
    this / thatRational
  }

  def reciprocal: Rational = {
    require(this.nonZero)
    normalize(this.denominator, this.numerator)
  }


  def ~(that: Rational): Boolean = {
    this.numerator * that.denominator == that.numerator * this.denominator
  }

  // Not useful, Stainless cannot handle custom equals correctly...
  override def equals(obj: Any): Boolean = {
    obj match {
      case that: Rational => this ~ that
      case that: BigIntExt => this ~ that
      case _ => false
    }
  }

  def !=(that: Rational): Boolean = {
    !(this ~ that)
  }

  def <(that: Rational): Boolean = {
    this.numerator * that.denominator < that.numerator * this.denominator
  }

  def <=(that: Rational): Boolean = {
    this.numerator * that.denominator <= that.numerator * this.denominator
  }

  def >(that: Rational): Boolean = {
    this.numerator * that.denominator > that.numerator * this.denominator
  }

  def >=(that: Rational): Boolean = {
    this.numerator * that.denominator >= that.numerator * this.denominator
  }

  def nonZero: Boolean = {
    numerator != 0
  }

  private def normalize(num: BigInt, den: BigInt): Rational = {
    require(den != 0)
    if (den < 0)
      Rational(-num, -den)
    else
      Rational(num, den)
  }
}

@library
object Rational {

  implicit def bigIntToRat(n: BigInt): Rational = Rational(n, 1)

  implicit def bigIntExtToRat(n: BigIntExt): Rational = Rational(n.underlying, 1)

  implicit def ratToBigInt(n: Rational): BigInt = n.numerator / n.denominator

  implicit def ratToBigIntExt(n: Rational): BigIntExt = BigIntExt(n.numerator / n.denominator)

  def apply(n: BigInt): Rational = Rational(n, 1)

  def unapply(n: Rational): Option[(BigInt, BigInt)] = Some((n.numerator, n.denominator))
}
