import stainless.lang._

object StringWrapperTest {
  def lengthTest(): OverflowInt = {
    StringWrapper("hello").length
  } ensuring(_ == OverflowInt(5))

  def sizeTest(): OverflowInt = {
    StringWrapper("hello").size
  } ensuring(_ == OverflowInt(5))

  def additionTest(): StringWrapper = {
    StringWrapper("hello") + StringWrapper(" world")
  } ensuring(_ == StringWrapper("hello world"))

  def additionWithOverflowIntTest(): StringWrapper = {
    StringWrapper("number: ") + OverflowInt(42)
  } ensuring(_ == StringWrapper("number: 42"))

  def charAtTest(): StringWrapper = {
    StringWrapper("hello").charAt(OverflowInt(1))
  } ensuring(_ == StringWrapper("e"))

  def substringTest(): StringWrapper = {
    StringWrapper("hello world").substring(OverflowInt(6))
  } ensuring(_ == StringWrapper("world"))

  def substringWithEndTest(): StringWrapper = {
    StringWrapper("hello world").substring(OverflowInt(0), OverflowInt(5))
  } ensuring(_ == StringWrapper("hello"))

  def lastCharacterTest(): StringWrapper = {
    StringWrapper("hello").last
  } ensuring(_ == StringWrapper("o"))
}