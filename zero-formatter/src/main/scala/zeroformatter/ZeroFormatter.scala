package zeroformatter

import java.io.ByteArrayOutputStream

object ZeroFormatter {

  def serialize[T](value: T)(implicit F: Formatter[T]): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    try {
      F.serialize(out, value)
      out.toByteArray()
    } finally {
      out.close()
    }
  }

  def deserialize[T](bytes: Array[Byte])(implicit F: Formatter[T]): T =
    F.deserialize(bytes, 0).value
}
