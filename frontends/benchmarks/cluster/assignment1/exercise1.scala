def double(x: Int): Int = x + x
def square(x: Int): Int = x * x

def power(x: Int, n: Int): Int =
  if (n == 0) { 1 }
  else { x * power(x, n - 1) }

object Test4 {
  /* Exercise 1 */
  def p(x: Int, y: Int): Int = square(x) + double(x * y) + power(y, 3) - 1
}

object Test2 {
  /* Exercise 1 */
  def p(x: Int, y: Int): Int = {
    return (power(x, 2) + 2 * x * y + power(y, 3) - 1)
  }
}