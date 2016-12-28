package zeroformatter

object ZeroFormatter {

  def serialize[T](value: T)(implicit F: Formatter[T]): Array[Byte] = {
    val bytes = Array.fill(F.length.getOrElse(0))(0.asInstanceOf[Byte])
    F.serialize(bytes, 0, value)._1
  }

  def deserialize[T](bytes: Array[Byte])(implicit F: Formatter[T]): T =
    F.deserialize(BinaryUtil.wrapByteArray(bytes), 0).value
}
