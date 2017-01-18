package zeroformatter

import java.nio.ByteBuffer

abstract class Decoder(var offset: Int) {

  def getByte(): Byte
  def getByte(offset: Int): Byte
  def getShort(): Short
  def getShort(offset: Int): Short
  def getInt(): Int
  def getInt(offset: Int): Int
  def getLong(): Long
  def getLong(offset: Int): Long
  def getFloat(): Float
  def getDouble(): Double
  def getChar(): Char
  def newOffset(o: Int): Decoder
}

final case class ArrayDecoder(buf: Array[Byte], private val _offset: Int) extends Decoder(_offset) {

  override def getByte(): Byte = {
    offset += 1
    buf(offset - 1)
  }

  override def getByte(o: Int): Byte = buf(o)

  override def getShort(): Short = {
    offset += 2
    val v1 = (buf(offset - 2) & 0xff).asInstanceOf[Int]
    val v2 = buf(offset - 1) << 8
    (v1 | v2).asInstanceOf[Short]
  }

  override def getShort(o: Int): Short = {
    val v1 = (buf(o) & 0xff).asInstanceOf[Int]
    val v2 = buf(o + 1) << 8
    (v1 | v2).asInstanceOf[Short]
  }

  override def getInt(): Int = {
    val v1 = (buf(offset) & 0xff).asInstanceOf[Int]
    val v2 = (buf(offset + 1) & 0xff) << 8
    val v3 = (buf(offset + 2) & 0xff) << 16
    val v4 = buf(offset + 3) << 24
    offset += 4
    v1 | v2 | v3 | v4
  }

  override def getInt(o: Int): Int = {
    val v1 = (buf(o) & 0xff).asInstanceOf[Int]
    val v2 = (buf(o + 1) & 0xff) << 8
    val v3 = (buf(o + 2) & 0xff) << 16
    val v4 = buf(o + 3) << 24
    v1 + v2 + v3 + v4
  }

  override def getLong(): Long = {
    (getInt().asInstanceOf[Long] & 0xffffffffL) | (getInt().asInstanceOf[Long] << 32)
  }

  override def getLong(o: Int): Long = {
    (getInt(o).asInstanceOf[Long] & 0xffffffffL) | (getInt(o).asInstanceOf[Long] << 32)
  }

  override def getFloat(): Float =
    java.lang.Float.intBitsToFloat(getInt())

  override def getDouble(): Double =
    java.lang.Double.longBitsToDouble(getLong())

  override def getChar(): Char = {
    offset += 2
    val v1 = (buf(offset - 2) & 0xff).asInstanceOf[Int]
    val v2 = buf(offset - 1) << 8
    (v1 | v2).toChar
  }

  override def newOffset(o: Int) = this.copy(_offset = o)
}

final case class BufferDecoder(buf: ByteBuffer, private val _offset: Int) extends Decoder(_offset) {

  override def getByte(): Byte = {
    offset += 1
    buf.get(offset - 1)
  }

  override def getByte(o: Int): Byte = buf.get(o)

  override def getShort(): Short = {
    offset += 2
    buf.getShort(offset - 2)
  }

  override def getShort(o: Int): Short = buf.getShort(o)

  override def getInt(): Int = {
    offset += 4
    buf.getInt(offset - 4)
  }

  override def getInt(o: Int): Int = buf.getInt(o)

  override def getLong(): Long = {
    offset += 8
    buf.getLong(offset - 8)
  }

  override def getLong(o: Int): Long = buf.getLong(o)

  override def getFloat(): Float = {
    offset += 4
    buf.getFloat(offset - 4)
  }

  override def getDouble(): Double = {
    offset += 8
    buf.getDouble(offset - 8)
  }

  override def getChar(): Char = {
    offset += 2
    buf.getChar(offset - 2)
  }

  override def newOffset(o: Int) = this.copy(_offset = o)
}
