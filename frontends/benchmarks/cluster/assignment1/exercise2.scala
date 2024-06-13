object Test4 {
  def sum(n: Int): Int = if (n == 0) {
    0
  } else {
    n + sum(n - 1)
  }
}

object Test14 {
  def sum(n: Int): Int =
    if (n == 1) {
      1
    } else {
      n + sum(n - 1)
    }
}

object Test36 {
  def sum(n: Int): Int = {
    val m = n - 1
    if (m == 0) {
      1
    } else {
      n + sum(m)
    }
  }
}

object Test2 {
  def sum(n: Int): Int = {
    var b = 0;
    // TODO
    for (a <- 0 to n) {
      b = b + a
    }
    return b
  }
}