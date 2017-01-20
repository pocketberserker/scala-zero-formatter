package zeroformatter

trait ZeroFormattable {

  def length: Option[Int]
}

abstract class Formatter[T] extends ZeroFormattable { self =>

  def default: T = null.asInstanceOf[T]

  def serialize(encoder: Encoder, offset: Int, value: T): Int

  def deserialize(decoder: Decoder): T

  def xmap[U](f: T => U, g: U => T): Formatter[U] = new Formatter[U] {
    override def length = self.length
    override def default = f(self.default)
    override def serialize(encoder: Encoder, offset: Int, value: U) =
      self.serialize(encoder, offset, g(value))
    override def deserialize(decoder: Decoder) =
      f(self.deserialize(decoder))
  }
}

object Formatter {

  @inline
  def apply[T](implicit F: Formatter[T]): Formatter[T] = F

  def serializeObjectField[T](encoder: Encoder, offset: Int, byteSize: Int, value: T, indexOffset: Int)(implicit F: Formatter[T]): Int = {
    val o = offset + byteSize
    val r = F.serialize(encoder, o, value)
    encoder.writeIntUnsafe(offset + indexOffset, o)
    byteSize + r
  }

  def serializeStructField[T](encoder: Encoder, offset: Int, byteSize: Int, value: T)(implicit F: Formatter[T]): Int = {
    byteSize + F.serialize(encoder, offset + byteSize, value)
  }

  def deserializeObjectField[T](decoder: Decoder, offset: Int, lastIndex: Int, index: Int, formatter: Formatter[T]): T =
    if(index > lastIndex) formatter.default
    else {
      val o = decoder.getInt(offset + 4 + 4 + 4 * index)
      if(o == 0) formatter.default
      else {
        decoder.offset = o
        formatter.deserialize(decoder)
      }
    }

  @inline
  def deserializeStructField[T](decoder: Decoder, formatter: Formatter[T]): T =
    formatter.deserialize(decoder)
}
