package zeroformatter

case class ObjectSerializerResult(encoder: Encoder, offset: Int, byteSize: Int)

object SerializeHelper {

  def serializeObjectField[T](encoder: Encoder, offset: Int, byteSize: Int, value: T, index: Int)(implicit F: Formatter[T]): Int = {
    val o = offset + byteSize
    val r = F.serialize(encoder, o, value)
    encoder.writeIntUnsafe(offset + 4 + 4 + 4 * index, o)
    byteSize + r
  }

  def serializeStructField[T](encoder: Encoder, offset: Int, byteSize: Int, value: T)(implicit F: Formatter[T]): Int = {
    byteSize + F.serialize(encoder, offset + byteSize, value)
  }
}
