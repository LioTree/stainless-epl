package stainless.frontends.dotc.epl

import java.security.MessageDigest

object Utils {
  def str2Int(str: String): Int = {
    val md = MessageDigest.getInstance("SHA-256")
    val hashBytes = md.digest(str.getBytes("UTF-8"))
    val shortHashBytes = hashBytes.take(3)

    BigInt(shortHashBytes).toInt
  }

  def extractFileName(path: String): String = {
    val regex = """(?:.*/)?([^/]+)\.scala$""".r
    path match {
      case regex(fileName) => fileName
      case _ => sys.error("Invalid file name")
    }
  }
}
