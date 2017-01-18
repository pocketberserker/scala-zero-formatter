package zeroformatter

trait ZeroFormattable {

  def length: Option[Int]
}

abstract class Formatter[T] extends ZeroFormattable { self =>

  def default: T = null.asInstanceOf[T]

  def serialize(bytes: Array[Byte], offset: Int, value: T): LazyResult[Array[Byte]]

  def deserialize(decoder: Decoder): T

  def xmap[U](f: T => U, g: U => T): Formatter[U] = new Formatter[U] {
    override def length = self.length
    override def default = f(self.default)
    override def serialize(bytes: Array[Byte], offset: Int, value: U) =
      self.serialize(bytes, offset, g(value))
    override def deserialize(decoder: Decoder) =
      f(self.deserialize(decoder))
  }
}

object Formatter {

  def apply[T](implicit F: Formatter[T]): Formatter[T] = F
}
