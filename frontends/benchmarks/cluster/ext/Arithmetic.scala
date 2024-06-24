// With translation from scala to pure scala
object Arithmetic {
  def pureBigIntExt(): Unit = {
    val a = 1
    val b = 2
    assert(a + b == 3)
    assert(a - b == -1)
    assert(a * b == 2)
    assert(a / b == 0)
    assert(a % b == 1)
    assert(a < b)
    assert(a + 1 <= b)
    assert(b > a)
    assert(b >= a + 1)
    assert(a + 1 == b)
    assert(a != b)
  }

  def pureRational(): Unit = {
    val a = 1.5
    val b = 2.5
//     Stainless cannot handle custom equals correctly
    assert(a + b == Rational(8,2))
    assert(a + b == 4)
    assert(a - b == -1)
    assert(a * b == 3.75)
    assert(a / b == 0.6)
//    assert(a % b == 1.5)
    assert(a < b)
    assert(a + 1 <= b)
    assert(b > a)
    assert(b >= a + 1)
    assert(a + 1 == b)
    assert(a != b)
  }

  def mix(): Unit = {
    val a = 123
    val b = 0.5
    val c = "abcd"
    assert(a + c == "123abcd")
    assert(c + a == "abcd123")
  }
}