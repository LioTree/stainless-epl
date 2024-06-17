object Test4 {
   def map12_withUpdate = ListMap[Int, String]() + (1 ->"a") + (2 -> "b")
//  def map12_withUpdate = ListMap.empty[Int, String] + (1 -> "a") + (2 -> "b")
}

object Test9 {
  def map12_withUpdate: ListMap[Int, String] = {
   val empty = ListMap[Int, String]();
//    val empty = ListMap.empty[Int, String];
    empty.updated(1, "a").updated(2, "b")
  }
}
