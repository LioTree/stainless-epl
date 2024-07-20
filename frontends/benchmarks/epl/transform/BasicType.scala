def intTest(): Int = {
  val a = 123
  val b = 456
  a + b
} ensuring(_ == 579)

def stringTest(): String = {
  val a = "Hello"
  val b = "World"
  a + b
} ensuring(_ == "HelloWorld")

def doubleTest(): Double = {
  val a = 123.0
  val b = 456.0
  a + b
} ensuring(_ == 579.0)

def doubleTest2(): Double = {
  val a = 123.1 // error
  val b = 456.0
  a + b
} ensuring(_ == 579.1)

def mixTest(): String = {
  val a = 123.0
  val b = "World"
  a + b
} ensuring(_ == "123World")