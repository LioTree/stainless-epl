object Test4 {
  /* Exercise 8 */
  def compose1[A, B, C](f: A => B, g: B => C)(x: A) = g(f(x))

  /* Exercise 9 */
  def compose[A, B, C](f: A => B, g: B => C) = (x: A) => g(f(x))
}

object Test13 {
  /* Exercise 8 */
  def compose1[A, B, C](f: A => B, g: B => C)(x: A) = {
    val m = f(x);
    val n = g(m);
    n
  }

  /* Exercise 9 */
  def compose[A, B, C](f: A => B, g: B => C) = (x: A) => g(f(x))
}