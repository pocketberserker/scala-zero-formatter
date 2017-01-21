package zeroformatter

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import spire.syntax.cfor._

abstract class Decoder(var offset: Int) {

  def buffer: Array[Byte]

  def readByte(): Byte
  def readByte(offset: Int): Byte
  def readShort(): Short
  def readShort(offset: Int): Short
  def readInt(): Int
  def readInt(offset: Int): Int
  def readLong(): Long
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
    offset += 1
    buf(offset - 1)
  }

  override def readByte(o: Int): Byte = buf(o)

  override def readShort(): Short = {
    offset += 2
    readShort(offset - 2)
  }

  override def readShort(o: Int): Short = {
    val v1 = (buf(o) & 0xff).asInstanceOf[Int]
    val v2 = buf(o + 1) << 8
    (v1 | v2).asInstanceOf[Short]
  }

  override def readInt(): Int = {
    offset += 4
    readInt(offset - 4)
  }

  override def readInt(o: Int): Int = {
    val v1 = (buf(o) & 0xff).asInstanceOf[Int]
    val v2 = (buf(o + 1) & 0xff) << 8
    val v3 = (buf(o + 2) & 0xff) << 16
    val v4 = buf(o + 3) << 24
    v1 | v2 | v3 | v4
  }

  override def readLong(): Long = {
    offset += 8
    readLong(offset - 8)
  }

  override def readLong(o: Int): Long = {
    (readInt(o).asInstanceOf[Long] & 0xffffffffL) | (readInt(o + 4).asInstanceOf[Long] << 32)
  }

  override def readFloat(): Float =
    java.lang.Float.intBitsToFloat(readInt())

  override def readDouble(): Double =
    java.lang.Double.longBitsToDouble(readLong())

  override def readChar(): Char = {
    offset += 2
    val v1 = (buf(offset - 2) & 0xff).asInstanceOf[Int]
    val v2 = buf(offset - 1) << 8
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
    offset += 1
    buf.get(offset - 1)
  }

  override def readByte(o: Int): Byte = buf.get(o)

  override def readShort(): Short = {
    offset += 2
    buf.getShort(offset - 2)
  }

  override def readShort(o: Int): Short = buf.getShort(o)

  override def readInt(): Int = {
    offset += 4
    buf.getInt(offset - 4)
  }

  override def readInt(o: Int): Int = buf.getInt(o)

  override def readLong(): Long = {
    offset += 8
    buf.getLong(offset - 8)
  }

  override def readLong(o: Int): Long = buf.getLong(o)

  override def readFloat(): Float = {
    offset += 4
    buf.getFloat(offset - 4)
  }

  override def readDouble(): Double = {
    offset += 8
    buf.getDouble(offset - 8)
  }

  override def readChar(): Char = {
    offset += 2
    buf.getChar(offset - 2)
  }

  override def newOffset(o: Int) = this.copy(_offset = o)
}
