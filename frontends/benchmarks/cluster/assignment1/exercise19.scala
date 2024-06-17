object Test4 {
  def list2map[K, V](l: List[(K, V)]): ListMap[K, V] =
    l match {
      case Nil => scala.collection.immutable.ListMap[K, V]()
      case (k, v) :: xs => scala.collection.immutable.ListMap(k -> v) ++ list2map(xs)
    }
}

object Test9 {
  def list2map[K, V](l: List[(K, V)]): ListMap[K, V] = l match {
    case Nil => scala.collection.immutable.ListMap[K, V]();
    case x :: y => (list2map(y)).updated(x._1, x._2)
  }
}

object Test12 {
  def list2map[K, V](l: List[(K, V)]): ListMap[K, V] = l match {
    case Nil => ListMap[K, V]()
    case p :: ps => list2map(ps).updated(p._1, p._2)
  }
}

object Test19 {
  def list2map[K, V](l: List[(K, V)]): ListMap[K, V] = l match {
    case Nil => ListMap[K, V]()
    case (k, v) :: xs => list2map(xs) + (k -> v)
  }
}

object Test33 {
  /* Exercise 19 */
  def list2map[K, V](l: List[(K, V)]): ListMap[K, V] = l match {
    case Nil => ListMap[K, V]()
    case (k, v) :: xs => ListMap[K, V]() + (k -> v) ++ list2map(xs)
  }
}
