object Test4 {
  def cycle(q:(Int,Int,Int)): (Int,Int,Int) = (q._2, q._3, q._1)
}

object Test2 {
  def cycle(q: (Int, Int, Int)): (Int, Int, Int) = {
    var first = q._2;
    var second = q._3;
    var third = q._1;
    return (first, second, third)
  }
}

object Test10 {
  def cycle(q: (Int, Int, Int)): (Int, Int, Int) = {
    val (x, y, z) = q;
    return (y, z, x);
  }
}