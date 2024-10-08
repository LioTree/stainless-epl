package stainless.lang

import stainless.annotation.library

import stainless.annotation._

import scala.language.implicitConversions

@library
case class StringWrapper(val underlying: String) {
  def +(that: StringWrapper): StringWrapper = StringWrapper(underlying + that.underlying)

  def +(that: OverflowInt): StringWrapper = this + that.toString

  def concat(that: StringWrapper): StringWrapper = this + that

  def ++(that: StringWrapper): StringWrapper = this + that

  def length: OverflowInt = OverflowInt(underlying.bigLength())

  def size: OverflowInt = length

  def charAt(index: OverflowInt): StringWrapper = StringWrapper(underlying.bigSubstring(index, index + OverflowInt(1)))

  def take(index: OverflowInt): StringWrapper = charAt(index)

  def apply(index: OverflowInt): StringWrapper = StringWrapper(underlying.bigSubstring(index, index + OverflowInt(1)))

  def substring(start: OverflowInt): StringWrapper = StringWrapper(underlying.bigSubstring(start))

  def substring(start: OverflowInt, end: OverflowInt): StringWrapper = StringWrapper(underlying.bigSubstring(start, end))

  def last: StringWrapper = StringWrapper(underlying.bigSubstring(this.length - OverflowInt(1), this.length))

  // StringWrapper corresponds not only to String but also to Char. Therefore, toStringWrapper is needed to correspond to Char::toString.
  def toStringWrapper: StringWrapper = this
}

@library
object StringWrapper {
  def apply(s: String): StringWrapper = new StringWrapper(s)

  def unapply(stringExt: StringWrapper): Option[String] = Some(stringExt.underlying)

  implicit def string2StringWrapper(b: String): StringWrapper = StringWrapper(b)

  implicit def stringWrapper2String(b: StringWrapper): String = b.underlying
}
