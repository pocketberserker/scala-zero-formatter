package zeroformatter

import java.nio.charset.StandardCharsets
import spire.syntax.cfor._
import BinaryUtil._

trait Encoder {

  def ensureCapacity(offset: Int, appendLength: Int): Unit

  def writeBoolUnsafe(offset:Int, value: Boolean): Int
  def writeBool(offset:Int, value: Boolean): Int
  def writeByte(offset:Int, value: Byte): Int
  def writeShortUnsafe(offset:Int, value: Short): Int
  def writeShort(offset:Int, value: Short): Int
  def writeIntUnsafe(offset:Int, value: Int): Int
  def writeInt(offset:Int, value: Int): Int
  def writeLongUnsafe(offset:Int, value: Long): Int
  def writeLong(offset:Int, value: Long): Int
  def writeChar(offset:Int, value: Char): Int
  def writeByteArray(offset:Int, value: Array[Byte]): Int
  def writeByteArray(offset: Int, value: Array[Byte], start: Int, len: Int): Int
  def toByteArray: Array[Byte]

  def writeString(offset: Int, value: String): Int =
    if(value == null) writeInt(offset, -1)
    else {
      val strBytes = value.getBytes(StandardCharsets.UTF_8)
      val len = strBytes.length
      ensureCapacity(offset, 4 + len)
      val intSize = writeIntUnsafe(offset, len)
      intSize + writeByteArray(offset + intSize, strBytes)
    }
}

final case class ArrayEncoder(private var buf: Array[Byte]) extends Encoder {

  override def ensureCapacity(offset: Int, appendLength: Int): Unit = {
    buf = BinaryUtil.ensureCapacity(buf, offset, appendLength)
  }

  override def toByteArray = buf

  override def writeBoolUnsafe(offset: Int, value: Boolean): Int = {
    buf(offset) = if(value) 1 else 0
    1
  }

  override def writeBool(offset: Int, value: Boolean): Int = {
    ensureCapacity(offset, 1)
    writeBoolUnsafe(offset, value)
  }

  override def writeByte(offset: Int, value: Byte): Int = {
    ensureCapacity(offset, 1)
    buf(offset) = value
    1
  }

  override def writeShortUnsafe(offset: Int, value: Short): Int = {
    buf(offset) = value.asInstanceOf[Byte]
    buf(offset + 1) = (value >>> 8).asInstanceOf[Byte]
    2
  }

  override def writeShort(offset: Int, value: Short): Int = {
    ensureCapacity(offset, 2)
    writeShortUnsafe(offset, value)
  }

  override def writeIntUnsafe(offset: Int, value: Int): Int = {
    @annotation.tailrec
    def go(pos: Int, v: Int): Int = {
      buf(offset + pos) = v.asInstanceOf[Byte]
      if(pos == 3) 4
      else go(pos + 1, v >>> 8)
    }
    go(0, value)
  }

  override def writeInt(offset: Int, value: Int): Int = {
    ensureCapacity(offset, 4)
    writeIntUnsafe(offset, value)
  }

  override def writeLongUnsafe(offset: Int, value: Long): Int = {
    @annotation.tailrec
    def go(pos: Int, v: Long): Int = {
      buf(offset + pos) = v.asInstanceOf[Byte]
      if(pos == 7) 8
      else go(pos + 1, v >>> 8)
    }
    go(0, value)
  }

  override def writeLong(offset: Int, value: Long): Int = {
    ensureCapacity(offset, 8)
    writeLongUnsafe(offset, value)
  }

  override def writeChar(offset: Int, value: Char): Int = {
    val byteSize = 2
    ensureCapacity(offset, byteSize)
    val cs = allocate(byteSize).putChar(value).array()
    cfor(0)(_ <= 1, _ + 1){ i => buf(offset + i) = cs(i) }
    byteSize
  }

  override def writeByteArray(offset: Int, value: Array[Byte]): Int =
    writeByteArray(offset, value, 0, value.length)

  override def writeByteArray(offset: Int, value: Array[Byte], start: Int, len: Int) = {
    ensureCapacity(offset, len)
    System.arraycopy(value, start, buf, offset, len)
    len
  }
}
