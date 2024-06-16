object Test2 {
  /* Exercise 13 */
  def reverse[A](l: List[A]): List[A] = {
    l match {
      case Nil   => Nil
      case x :: xs => reverse(xs) :+ x
    }
  }
}

object Test4 {
  def reverse[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
       case x :: xs => reverse(xs) ::: List(x)
//      case (x :: xs) => reverse(xs) ++ List(x)
    }
  }
}
