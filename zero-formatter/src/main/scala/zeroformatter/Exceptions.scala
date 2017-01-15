package zeroformatter

case class FormatException(offset: Int, message: String) extends RuntimeException(s"[$offset] $message")
