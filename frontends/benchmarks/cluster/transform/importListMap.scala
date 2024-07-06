object Test1 {
  import scala.collection.immutable.ListMap
  val x = ListMap(1 -> 2)
}

object Test2 {
  val x = scala.collection.immutable.ListMap(1 -> 2)
}

object Test3 {
  import scala.collection.immutable.ListMap
  val x:scala.collection.immutable.ListMap[Int,Int] = ListMap.empty[Int,Int]
}