/* Copyright 2009-2021 EPFL, Lausanne */

package stainless

import stainless.annotation._
import stainless.lang.StaticChecks._

import scala.language.implicitConversions

package object math {

  /** Disable overflow checks within `body`.
   *
   * This is equivalent to setting `--strict-arithmetic=false` for `body` only.
   */
  @ignore
  def wrapping[A](body: A): A = body

  @library
  def min(i1: Int, i2: Int) = if (i1 <= i2) i1 else i2

  @library
  def max(i1: Int, i2: Int) = if (i1 >= i2) i1 else i2

  @library
  def min(i1: BigInt, i2: BigInt) = if (i1 <= i2) i1 else i2

  @library
  def max(i1: BigInt, i2: BigInt) = if (i1 >= i2) i1 else i2

  @library
  def max(i1: BigInt, i2: BigInt, i3: BigInt) = if (i1 >= i2) i1 else if (i2 >= i3) i2 else i3

  @library
  def min(i1: Nat, i2: Nat) = if (i1 <= i2) i1 else i2

  @library
  def max(i1: Nat, i2: Nat) = if (i1 >= i2) i1 else i2

  @library
  def abs(n: BigInt) = if (n < 0) -n else n

  @library
  def sqrt(x: BigInt): BigInt = {
    def next(n: BigInt, i: BigInt): BigInt = (n + i / n) / 2

    val one = BigInt(1)
    var n = one
    var n1 = next(n, x)

    while (abs(n1 - n) > one) {
      n = n1
      n1 = next(n, x)
    }

    while (n1 * n1 > x) {
      n1 -= one
    }

    n1
  }

  @library
  implicit def bigIntToNat(b: BigInt): Nat = {
    require(b >= 0)
    Nat(b)
  }
}

