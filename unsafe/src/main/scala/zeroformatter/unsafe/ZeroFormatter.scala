package zeroformatter
package unsafe

object ZeroFormatter {

  def serialize[T](value: T)(implicit F: Formatter[T]): Array[Byte] = {
    val encoder = UnsafeEncoder(new Array[Byte](F.length.getOrElse(0)))
    val byteSize = F.serialize(encoder, 0, value)
    val result = encoder.toByteArray
    if(result.length != byteSize) BinaryUtil.resize(result, byteSize)
    else result
  }

  def deserialize[T](bytes: Array[Byte])(implicit F: Formatter[T]): T =
    F.deserialize(UnsafeDecoder(bytes, 0))
}
