package zeroformatter

import java.io.{EOFException, ByteArrayOutputStream}
import java.nio.{ByteBuffer, ByteOrder}

trait ZeroFormattable {

  def length: Option[Int]
}

case class DeserializeResult[T](value: T, byteSize: Int)

abstract class Formatter[T] extends ZeroFormattable {

  // TODO: offet
  def serialize(buf: ByteArrayOutputStream, value: T): Int

  def deserialize(bytes: Array[Byte], offset: Int): DeserializeResult[T]
}

object Formatter {

  private[this] def wrapByteArray(bytes: Array[Byte]): ByteBuffer =
    ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)

  private[this] def allocate(capacity: Int): ByteBuffer =
    ByteBuffer.allocate(capacity).order(ByteOrder.LITTLE_ENDIAN)

  implicit val boolFormatter: Formatter[Boolean] = new Formatter[Boolean] {
    override val length = Some(1)
    override def serialize(buf: ByteArrayOutputStream, value: Boolean) = {
      buf.write(if(value) 1 else 0)
      1
    }
    override def deserialize(bytes: Array[Byte], offset: Int) = {
      val c = bytes(offset)
      if (c < 0) throw new EOFException()
      else DeserializeResult(c != 0, 1)
    }
  }

  implicit val byteFormatter: Formatter[Byte] = new Formatter[Byte] {
    override val length = Some(1)
    override def serialize(buf: ByteArrayOutputStream, value: Byte) = {
      buf.write(value)
      1
    }
    override def deserialize(bytes: Array[Byte], offset: Int) = {
      val c = bytes(offset)
      if (c < 0) throw new EOFException()
      else DeserializeResult(c, 1)
    }
  }

  implicit val shortFormatter: Formatter[Short] = new Formatter[Short] {
    override val length = Some(2)
    override def serialize(buf: ByteArrayOutputStream, value: Short) = {
      buf.write(value.asInstanceOf[Byte])
      buf.write((value >>> 8).asInstanceOf[Byte])
      2
    }
    override def deserialize(bytes: Array[Byte], offset: Int) =
      DeserializeResult(wrapByteArray(bytes).getShort(offset), 2)
  }

  implicit val intFormatter: Formatter[Int] = new Formatter[Int] {
    override val length = Some(4)
    override def serialize(buf: ByteArrayOutputStream, value: Int) = {
      buf.write(value.asInstanceOf[Byte])
      buf.write((value >>> 8).asInstanceOf[Byte])
      buf.write((value >>> 16).asInstanceOf[Byte])
      buf.write((value >>> 24).asInstanceOf[Byte])
      4
    }
    override def deserialize(bytes: Array[Byte], offset: Int) =
      DeserializeResult(wrapByteArray(bytes).getInt(offset), 4)
  }

  implicit val longFormatter: Formatter[Long] = new Formatter[Long] {
    override val length = Some(8)
    override def serialize(buf: ByteArrayOutputStream, value: Long) = {
      buf.write(value.asInstanceOf[Byte])
      buf.write((value >>> 8).asInstanceOf[Byte])
      buf.write((value >>> 16).asInstanceOf[Byte])
      buf.write((value >>> 24).asInstanceOf[Byte])
      buf.write((value >>> 32).asInstanceOf[Byte])
      buf.write((value >>> 40).asInstanceOf[Byte])
      buf.write((value >>> 48).asInstanceOf[Byte])
      buf.write((value >>> 56).asInstanceOf[Byte])
      8
    }
    override def deserialize(bytes: Array[Byte], offset: Int) =
      DeserializeResult(wrapByteArray(bytes).getLong(offset), 8)
  }

  implicit val floatFormatter: Formatter[Float] = new Formatter[Float] {
    override val length = Some(4)
    override def serialize(buf: ByteArrayOutputStream, value: Float) =
      intFormatter.serialize(buf, java.lang.Float.floatToIntBits(value))
    override def deserialize(bytes: Array[Byte], offset: Int) =
      DeserializeResult(wrapByteArray(bytes).getFloat(offset), 4)
  }

  implicit val doubleFormatter: Formatter[Double] = new Formatter[Double] {
    override val length = Some(8)
    override def serialize(buf: ByteArrayOutputStream, value: Double) =
      longFormatter.serialize(buf, java.lang.Double.doubleToLongBits(value))
    override def deserialize(bytes: Array[Byte], offset: Int) =
      DeserializeResult(wrapByteArray(bytes).getDouble(offset), 8)
  }

  implicit val charFormatter: Formatter[Char] = new Formatter[Char] {
    override val length = Some(2)
    override def serialize(buf: ByteArrayOutputStream, value: Char) = {
      val byteSize = 2
      buf.write(allocate(byteSize).putChar(value).array(), 0, byteSize)
      byteSize
    }
    override def deserialize(bytes: Array[Byte], offset: Int) =
      DeserializeResult(wrapByteArray(bytes).getChar(offset), 2)
  }
}
