object Test4 {
  val presidentListMap = scala.collection.immutable.ListMap(41 -> "George H. W. Bush", 42 -> "Bill Clinton", 43 -> "George W. Bush", 44 -> "Barack Obama", 45 -> "Donald J. Trump")
}

object Test3 {
  val presidentListMap: ListMap[Int, String] =
    ListMap(41 -> "George H. W. Bush",
      42 -> "Bill Clinton",
      43 -> "George W. Bush",
      44 -> "Barack Obama",
      45 -> "Donald J. Trump")
}