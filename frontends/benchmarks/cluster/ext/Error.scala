object Error {
  // sys.error should be seem as equivalent
  def lookup[K, V](m: List[(K, V)], k: K): V = {
    m match {
      case Nil => sys.error("yyyyy")
      case (x, y) :: xs =>
        if (x == k) {
          y
        } else lookup(xs, k)
    }
  }

  def lookup2[K, V](m: List[(K, V)], k: K): V = {
    m match {
      case Nil => sys.error("xxxx")
      case x :: xs =>
        if (x._1 == k) {
          x._2
        } else {
          lookup(xs, k)
        }
    }
  }
}