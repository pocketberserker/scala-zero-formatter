package zeroformatter

case class ObjectSerializerResult(bytes: Array[Byte], offset: Int, byteSize: Int)

object SerializeHelper {

  def serializeObjectField[T](acc: ObjectSerializerResult, value: T, index: Int)(implicit F: Formatter[T]): ObjectSerializerResult = {
    val o = acc.offset + acc.byteSize
    val r = F.serialize(acc.bytes, o, value)
    ObjectSerializerResult(BinaryUtil.writeIntUnsafe(r.value, acc.offset + 4 + 4 + 4 * index, o), acc.offset, acc.byteSize + r.byteSize)
  }

  def serializeStructField[T](acc: ObjectSerializerResult, value: T)(implicit F: Formatter[T]): ObjectSerializerResult = {
    val r = F.serialize(acc.bytes, acc.offset, value)
    ObjectSerializerResult(r.value, acc.offset + r.byteSize, acc.byteSize + r.byteSize)
  }
}
