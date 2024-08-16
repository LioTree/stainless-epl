package epl.assn1

import stainless.annotation.*
import stainless.lang.*

package object framework {
  @library
  def power(x: OverflowInt, n: OverflowInt): OverflowInt = {
    require(OverflowInt(0) <= n && n <= OverflowInt(100))
    if (n == OverflowInt(0))
      OverflowInt(1)
    else
      x * power(x,n - OverflowInt(1))
  }

  @library
  sealed abstract class Colour
  @library
  case class Red() extends Colour
  @library
  case class Green() extends Colour
  @library
  case class Blue() extends Colour

  @library
  sealed abstract class Shape
  @library
  case class Circle(r: BigInt, x: BigInt, y: BigInt) extends Shape {
    require(r >= 0 && x >= r && y >= r)
  }
  @library
  case class Rectangle(llx: BigInt, lly: BigInt, w: BigInt, h: BigInt) extends Shape {
    require(llx >= 0 && lly >= 0 && w >= 0 && h >= 0)
  }
}