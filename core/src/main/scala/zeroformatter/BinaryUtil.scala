package zeroformatter

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets
import spire.syntax.cfor._

object BinaryUtil {

  private[zeroformatter] def wrapByteArray(bytes: Array[Byte]): ByteBuffer =
    ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)

  private[this] def allocate(capacity: Int): ByteBuffer =
    ByteBuffer.allocate(capacity).order(ByteOrder.LITTLE_ENDIAN)

  private[zeroformatter] def resize(array: Array[Byte], newSize: Int): Array[Byte] = {
    if(array.length != newSize) {
      val array2 = new Array[Byte](newSize)
      val l = array.length
      array.copyToArray(array2, 0, if(l > newSize) newSize else l)
      array2
    }
    else array
  }

  private[zeroformatter] def ensureCapacity(bytes: Array[Byte], offset: Int, appendLength: Int): Array[Byte] = {
    val newLength = offset + appendLength

    val current = bytes.length
    if(newLength > current) {
      val num =
        if(newLength < 256) 256
        else if(newLength < current * 2) current * 2
        else newLength
      resize(bytes, num)
    }
    else bytes
  }

  def writeBool(bytes: Array[Byte], offset: Int, value: Boolean): Array[Byte] = {
    val bs = ensureCapacity(bytes, offset, 1)
    bs(offset) = if(value) 1 else 0
    bs
  }

  def writeByte(bytes: Array[Byte], offset: Int, value: Byte): Array[Byte] = {
    val bs = ensureCapacity(bytes, offset, 1)
    bs(offset) = value
    bs
  }

  def writeShort(bytes: Array[Byte], offset: Int, value: Short): Array[Byte] = {
    val bs = ensureCapacity(bytes, offset, 2)
    bs(offset) = value.asInstanceOf[Byte]
    bs(offset + 1) = (value >>> 8).asInstanceOf[Byte]
    bs
  }

  def writeIntUnsafe(bytes: Array[Byte], offset: Int, value: Int): Array[Byte] = {
    @annotation.tailrec
    def go(bs: Array[Byte], pos: Int, v: Int): Array[Byte] = {
      bs(offset + pos) = v.asInstanceOf[Byte]
      if(pos == 3) bs
      else go(bs, pos + 1, v >>> 8)
    }
    go(bytes, 0, value)
  }

  @inline
  def writeInt(bytes: Array[Byte], offset: Int, value: Int): Array[Byte] =
    writeIntUnsafe(ensureCapacity(bytes, offset, 4), offset, value)

  def writeLong(bytes: Array[Byte], offset: Int, value: Long): Array[Byte] = {
    @annotation.tailrec
    def go(bs: Array[Byte], pos: Int, v: Long): Array[Byte] = {
      bs(offset + pos) = v.asInstanceOf[Byte]
      if(pos == 7) bs
      else go(bs, pos + 1, v >>> 8)
    }
    go(ensureCapacity(bytes, offset, 8), 0, value)
  }

  def writeChar(bytes: Array[Byte], offset: Int, value: Char): Array[Byte] = {
    val byteSize = 2
    val bs = ensureCapacity(bytes, offset, byteSize)
    val cs = allocate(byteSize).putChar(value).array()
    cfor(0)(_ <= 1, _ + 1){ i => bs(offset + i) = cs(i) }
    bs
  }

  private[this] final val intSize = 4

  def writeString(bytes: Array[Byte], offset: Int, value: String): LazyResult[Array[Byte]] =
    if(value == null) {
      LazyResult(writeInt(bytes, offset, -1), intSize)
    }
    else {
      val strBytes = value.getBytes(StandardCharsets.UTF_8)
      val len = strBytes.length
      val bs = writeIntUnsafe(ensureCapacity(bytes, offset, intSize + len), offset, len)
      cfor(0)(_ < len, _ + 1){ i => bs(offset + intSize + i) = strBytes(i) }
      LazyResult(bs, intSize + len)
    }

  def readString(decoder: Decoder): String = {
    val len = decoder.getInt()
    if(len == -1) null
    else if(len < -1) throw FormatException(decoder.offset, "Invalid string length.")
    else {
      val bytes = new Array[Byte](len)
      cfor(0)(_ < len, _ + 1){ i => bytes(i) = decoder.getByte() }
      new String(bytes, StandardCharsets.UTF_8)
    }
  }
}
