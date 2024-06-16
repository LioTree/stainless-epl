object Test4 {
  /* Exercise 15 */
  def update[K, V](m: List[(K, V)], k: K, v: V): List[(K, V)] =
    m match {
      case Nil                      => List((k, v))
      case (x, y) :: xs if (x == k) => (x, v) :: xs
      case x :: xs                  => x :: update(m, k, v)
    }
}

object Test23 {
  /* Exercise 15 */
  def update[K, V](m: List[(K, V)], k: K, v: V): List[(K, V)] = {
    m match {
      case Nil => List((k, v))
      case (ck, cv) :: rest => {
        if (ck == k) {
          return (k, v) :: rest
        } else {
          return (ck, cv) :: update(rest, k, v)
        }
      }
    }
  }
}
