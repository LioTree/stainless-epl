package stainless.lang

import stainless.annotation.library

import stainless.annotation._

import scala.language.implicitConversions

@library
case class StringExt(val underlying: String) {
  def +(that: StringExt): StringExt = StringExt(underlying + that.underlying)

  def +(that: BigIntExt): StringExt = this + that.toStringExt

  def length: BigIntExt = BigIntExt(underlying.bigLength())

  // Because of stainless bug, these three methods cannot be used
  @extern @pure
  def substring(start: BigIntExt): StringExt = {
//    StringExt(underlying.bigSubstring(start))
    StringExt("")
  }

  @extern @pure
  def substring(start: BigIntExt, end: BigIntExt): StringExt = {
//    StringExt(underlying.bigSubstring(start, end))
    StringExt("")
  }

  @extern @pure
  def last: StringExt = {
//    StringExt(underlying.bigSubstring(this.length - BigIntExt(scala.BigInt(1)), this.length))
    StringExt("")
  }

  def contentEquals(that: StringExt): Boolean = underlying == that.underlying
}

@library
object StringExt {
  def apply(s: String): StringExt = new StringExt(s)

  def unapply(stringExt: StringExt): Option[String] = Some(stringExt.underlying)

  implicit def stringToStringExt(b: String): StringExt = StringExt(b)

  implicit def stringExtToString(b: StringExt): String = b.underlying
}
