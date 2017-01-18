package zeroformatter

object ZeroFormatter {

  def serialize[T](value: T)(implicit F: Formatter[T]): Array[Byte] = {
    val bytes = new Array[Byte](F.length.getOrElse(0))
    val r = F.serialize(bytes, 0, value)
    if(r.value.length != r.byteSize) BinaryUtil.resize(r.value, r.byteSize) else r.value
  }

  def deserialize[T](bytes: Array[Byte])(implicit F: Formatter[T]): T =
    F.deserialize(ArrayDecoder(bytes, 0))
}
