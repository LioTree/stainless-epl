package epl.assn1

import stainless.annotation.*
import stainless.lang.*
import epl.assn1.framework.*

package object fake {
  @library
  def boundingBox(s: Shape): Rectangle = s match {
    case Rectangle(lrx, lry, w, h) => Rectangle(lrx, lry, w, h)
    case Circle(r, x, y) => Rectangle(x - r, y - r, 2 * r, 2 * r)
  }
}