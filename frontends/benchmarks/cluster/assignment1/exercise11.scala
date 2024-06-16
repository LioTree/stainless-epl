object Test4 {
  /* Exercise 11 */
  def map[A, B](f: A => B, l: List[A]): List[B] =
    l match {
      case Nil => Nil
      case x :: xs => f(x) :: map(f, xs)
    }
}

object Test2 {
  /* Exercise 11 */
  def map[A, B](f: A => B, l: List[A]): List[B] =
    l match {
      case x :: xs => f(x) :: map(f, xs)
      case Nil     => Nil
    }
}
