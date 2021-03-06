package zeroformatter

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import spire.syntax.cfor._

abstract class Decoder(var offset: Int) {

  def buffer: Array[Byte]

  def readByte(): Byte
  def readByte(offset: Int): Byte
  def readShort(): Short = {
    val r = readShort(offset)
    offset += 2
    r
  }
  def readShort(offset: Int): Short
  def readInt(): Int = {
    val r = readInt(offset)
    offset += 4
    r
  }
  def readInt(offset: Int): Int
  def readLong(): Long = {
    val r = readLong(offset)
    offset += 8
    r
  }
  def readLong(offset: Int): Long
  def readFloat(): Float
  def readDouble(): Double
  def readChar(): Char
  def newOffset(o: Int): Decoder

  def readBool(): Boolean = readByte() match {
    case 1 => true
    case 0 => false
    case v => throw FormatException(offset - 1, s"Invalid Boolean byte($v).")
  }

  def readString(): String = {
    val len = readInt()
    if(len == -1) null
    else if(len < -1) throw FormatException(offset - 4, s"Invalid string length($len).")
    else {
      val bytes = new Array[Byte](len)
      cfor(0)(_ < len, _ + 1){ i => bytes(i) = readByte() }
      new String(bytes, StandardCharsets.UTF_8)
    }
  }
}

final case class ArrayDecoder(buf: Array[Byte], private val _offset: Int) extends Decoder(_offset) {

  override val buffer = buf

  override def readByte(): Byte = {
    val r = buf(offset)
    offset += 1
    r
  }

  override def readByte(o: Int): Byte = buf(o)

  override def readShort(o: Int): Short = {
    val v1 = (buf(o) & 0xff).asInstanceOf[Int]
    val v2 = buf(o + 1) << 8
    (v1 | v2).asInstanceOf[Short]
  }

  override def readInt(o: Int): Int =
    (buf(o) & 0xff) | ((buf(o + 1) & 0xff) << 8) | ((buf(o + 2) & 0xff) << 16) | (buf(o + 3) << 24)

  override def readLong(o: Int): Long = {
    (readInt(o).asInstanceOf[Long] & 0xffffffffL) | (readInt(o + 4).asInstanceOf[Long] << 32)
  }

  override def readFloat(): Float =
    java.lang.Float.intBitsToFloat(readInt())

  override def readDouble(): Double =
    java.lang.Double.longBitsToDouble(readLong())

  override def readChar(): Char = {
    val v1 = (buf(offset) & 0xff).asInstanceOf[Int]
    val v2 = buf(offset + 1) << 8
    offset += 2
    (v1 | v2).toChar
  }

  override def newOffset(o: Int) = this.copy(_offset = o)

  override def readString(): String = {
    val len = readInt()
    if(len == -1) null
    else if(len < -1) throw FormatException(offset - 4, s"Invalid string length($len).")
    else {
      val r = new String(buf, offset, len, StandardCharsets.UTF_8)
      offset += len
      r
    }
  }
}

final case class BufferDecoder(buf: ByteBuffer, private val _offset: Int) extends Decoder(_offset) {

  override val buffer = buf.array()

  override def readByte(): Byte = {
    val r = buf.get(offset)
    offset += 1
    r
  }

  override def readByte(o: Int): Byte = buf.get(o)

  override def readShort(o: Int): Short = buf.getShort(o)

  override def readInt(o: Int): Int = buf.getInt(o)

  override def readLong(o: Int): Long = buf.getLong(o)

  override def readFloat(): Float = {
    val r = buf.getFloat(offset)
    offset += 4
    r
  }

  override def readDouble(): Double = {
    val r = buf.getDouble(offset)
    offset += 8
    r
  }

  override def readChar(): Char = {
    val r = buf.getChar(offset)
    offset += 2
    r
  }

  override def newOffset(o: Int) = this.copy(_offset = o)
}
