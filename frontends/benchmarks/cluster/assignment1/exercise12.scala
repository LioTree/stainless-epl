object Test4 {
  /* Exercise 12 */
  def filter[A](f: A => Boolean, l: List[A]): List[A] =
    l match {
      case Nil             => Nil
      case x :: xs if f(x) => x :: filter(f, xs)
      case _ :: xs         => filter(f, xs)
    }
}

object Test2 {
  /* Exercise 12 */
  def filter[A](f: A => Boolean, l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case x :: xs =>
        if (f(x)) x :: filter(f, xs)
        else filter(f, xs)
    }
}
