package zeroformatter

import java.nio.ByteBuffer

abstract class Enum[
  @specialized(Byte, Short, Int, Long)T: Formatter
] extends ZeroFormattable {

  def label: T

  private[this] val formatter = implicitly[Formatter[T]]

  override val length: Option[Int] = formatter.length

  def serialize(bytes: Array[Byte], offset: Int): LazyResult[Array[Byte]] =
    formatter.serialize(bytes, offset, label)

  def check(buf: ByteBuffer, offset: Int): Option[Int] = {
    val r = formatter.deserialize(buf, offset)
    if(r.value == label) Some(r.byteSize)
    else None
  }
}
