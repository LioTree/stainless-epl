object Test4 {
  /* Exercise 16 */
  def keys[K, V](m: List[(K, V)]): List[K] =
    m match {
      case Nil          => Nil
      case (x, y) :: xs => x :: keys(xs)
    }
}

object Test9 {
  /* Exercise 16 */
  def keys[K, V](m: List[(K, V)]): List[K] =
    m match {
      case Nil    => Nil
      case x :: y => x._1 :: keys(y)
    }
}
