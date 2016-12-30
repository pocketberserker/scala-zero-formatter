package zeroformatter

object ZeroFormatter {

  def serialize[T](value: T)(implicit F: Formatter[T]): Array[Byte] = {
    val bytes = Array.fill(F.length.getOrElse(0))(0.asInstanceOf[Byte])
    val (rs, size) = F.serialize(bytes, 0, value)
    if(rs.length != size) BinaryUtil.resize(rs, size) else rs
  }

  def deserialize[T](bytes: Array[Byte])(implicit F: Formatter[T]): T =
    F.deserialize(BinaryUtil.wrapByteArray(bytes), 0).value
}
