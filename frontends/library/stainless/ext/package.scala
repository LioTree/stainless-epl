package stainless

import stainless.annotation.library
import stainless.collection.List
import stainless.collection.ListMap
import stainless.lang.decreases

package object ext {
  @library
  def intToString(n: Int): String = {
    if (n == 0) return "0"

    var number = n
    val isNegative = number < 0
    if (isNegative) number = -number

    var digits = List[String]()

    while (number > 0) {
      decreases(number)
      val digit = number % 10
      val digitStr = digit match {
        case 0 => "0"
        case 1 => "1"
        case 2 => "2"
        case 3 => "3"
        case 4 => "4"
        case 5 => "5"
        case 6 => "6"
        case 7 => "7"
        case 8 => "8"
        case 9 => "9"
      }
      digits = digitStr :: digits
      number /= 10
    }

    if (isNegative) digits = "-" :: digits

    List.mkString(digits, "", x => x)
  }

  @library
  def plus(x: Int, y: Int): Int = {
    x + y
  }

  @library
  def plus(x: String, y: Int): String = {
    x + intToString(y)
  }

  @library
  def plus(x: Int, y: String): String = {
    intToString(x) + y
  }

  @library
  def plus(x: String, y: String): String = {
    x + y
  }

  @library
  def plus[A,B](x: ListMap[A,B], y:(A,B)): ListMap[A,B] = {
    x + y
  }

  @library
  def plus[A,B](x: ListMap[A,B], y:List[(A,B)]): ListMap[A,B] = {
    x ++ y
  }

  @library
  def toString(x: Int): String = {
    intToString(x)
  }
}
