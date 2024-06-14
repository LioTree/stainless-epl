abstract class Shape
case class Circle(r: Double, x: Double, y: Double) extends Shape
case class Rectangle(llx: Double, lly: Double, w:Double, h:Double) extends Shape

def center(s: Shape): (Double,Double) = s match {
  case Rectangle(llx,lly,w,h) => (llx+w/2, lly+h/2)
  case Circle(r,x,y) => (x,y)
}

object Test4 {
  def boundingBox(s: Shape): Rectangle = s match {
    case Rectangle(x, y, w, h) => Rectangle(x, y, w, h)
    case Circle(r, x, y) => Rectangle(x - r, y - r, r + r, r + r)
  }

  def mayOverlap(s1: Shape, s2: Shape): Boolean = {
    val b1: Rectangle = boundingBox(s1);
    val b2: Rectangle = boundingBox(s2);

    val x1: Boolean = b2.llx > b1.llx + b1.w
    val x2: Boolean = b2.lly > b1.lly + b1.h
    val x3: Boolean = b1.llx > b2.llx + b2.w
    val x4: Boolean = b1.lly > b2.lly + b2.h

    return !(x1 || x2 || x3 || x4)
  }
}

object Test2 {
  def boundingBox(s: Shape): Rectangle = s match {
    case Rectangle(llx, lly, w, h) => Rectangle(llx, lly, w, h);
    case Circle(r, x, y) => Rectangle(x - r, y - r, 2 * r, 2 * r);
  }

  /* Exercise 7 */
  def mayOverlap(s1: Shape, s2: Shape): Boolean = {
    val rect1 = boundingBox(s1)
    val rect2 = boundingBox(s2)

    !(rect1.llx + rect1.w < rect2.llx || rect2.llx + rect2.w < rect1.llx ||
      rect1.lly + rect1.h < rect2.lly || rect2.lly + rect2.h < rect1.lly)
  }
}