def forTest(): Int = {
  val list = List(1, 2, 3, 4, 5)
  var result = 0
  for (i <- list) {
    result += i
  }
  result
} ensuring(_ == 15)

def toTest(): Int = {
  val n = 5
  var result = 0
  for (i <- 0 to n) {
    result += i
  }
  result
} ensuring(_ == 15)

def untilTest(): Int = {
  val n = 6
  var result = 0
  for (i <- 0 until n) {
    result += i
  }
  result
} ensuring(_ == 15)