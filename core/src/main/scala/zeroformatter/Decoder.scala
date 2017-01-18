package zeroformatter

import java.nio.ByteBuffer

final case class Decoder(buf: ByteBuffer, var offset: Int) {

  def getByte(): Byte = {
    offset += 1
    buf.get(offset - 1)
  }

  def getByte(o: Int): Byte = buf.get(o)

  def getShort(): Short = {
    offset += 2
    buf.getShort(offset - 2)
  }

  def getShort(o: Int): Short = buf.getShort(o)

  def getInt(): Int = {
    offset += 4
    buf.getInt(offset - 4)
  }

  def getInt(o: Int): Int = buf.getInt(o)

  def getLong(): Long = {
    offset += 8
    buf.getLong(offset - 8)
  }

  def getLong(o: Int): Long = buf.getLong(o)

  def getFloat(): Float = {
    offset += 4
    buf.getFloat(offset - 4)
  }

  def getDouble(): Double = {
    offset += 8
    buf.getDouble(offset - 8)
  }

  def getChar(): Char = {
    offset += 2
    buf.getChar(offset - 2)
  }
}
