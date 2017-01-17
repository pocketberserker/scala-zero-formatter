package zeroformatter

import java.nio.ByteBuffer

trait ZeroFormattable {

  def length: Option[Int]
}

abstract class Formatter[T] extends ZeroFormattable { self =>

  def default: T = null.asInstanceOf[T]

  def serialize(bytes: Array[Byte], offset: Int, value: T): LazyResult[Array[Byte]]

  def deserialize(buf: ByteBuffer, offset: Int): LazyResult[T]

  def xmap[U](f: T => U, g: U => T): Formatter[U] = new Formatter[U] {
    override def length = self.length
    override def default = f(self.default)
    override def serialize(bytes: Array[Byte], offset: Int, value: U) =
      self.serialize(bytes, offset, g(value))
    override def deserialize(buf: ByteBuffer, offset: Int) = {
      val r = self.deserialize(buf, offset)
      LazyResult(f(r.value), r.byteSize)
    }
  }
}

object Formatter {

  def apply[T](implicit F: Formatter[T]): Formatter[T] = F
}
