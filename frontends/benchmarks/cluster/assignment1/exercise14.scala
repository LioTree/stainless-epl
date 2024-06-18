object Test4 {
  /* Exercise 14 */
  def lookup[K, V](m: List[(K, V)], k: K): V = m match {
    case Nil => sys.error("no matching key")
    case (x, y) :: xs => if (x == k) {
      y
    } else lookup(xs, k)
  }
}

object Test11 {
  /* Exercise 14 */
  def lookup[K, V](m: List[(K, V)], k: K): V = m match {
    case Nil => sys.error("Key not in list")
    case x :: xs => if (x._1 == k) {
      x._2
    } else {
      lookup(xs, k)
    }
  }
}