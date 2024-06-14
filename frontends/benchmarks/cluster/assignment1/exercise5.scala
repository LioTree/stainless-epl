abstract class Colour
case class Red() extends Colour
case class Green() extends Colour
case class Blue() extends Colour

object Test4 {
  def favouriteColour(c: Colour): Boolean = c match {
    case Red() => false
    case Blue() => false
    case Green() => true
  }
}

object Test2 {
  def favouriteColour(c: Colour): Boolean = c match {
    case Red() => false
    case Blue() => true
    case Green() => false
  }
}

object Test11 {
  def favouriteColour(c: Colour): Boolean = c match {
    case Red() => false
    case Blue() => false
    case Green() => true
  }
}