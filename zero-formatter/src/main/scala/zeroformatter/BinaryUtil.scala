package zeroformatter

import java.nio.{ByteBuffer, ByteOrder}

object BinaryUtil {

  private[zeroformatter] def wrapByteArray(bytes: Array[Byte]): ByteBuffer =
    ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)

  private[this] def allocate(capacity: Int): ByteBuffer =
    ByteBuffer.allocate(capacity).order(ByteOrder.LITTLE_ENDIAN)

  private[this] def resize(array: Array[Byte], newSize: Int): Array[Byte] = {
    if(array.length != newSize) {
      val array2 = Array.fill(newSize)(0.asInstanceOf[Byte])
      val l = array.length
      array.copyToArray(array2, 0, if(l > newSize) newSize else l)
      array2
    }
    else array
  }

  private[this] def ensureCapacity(bytes: Array[Byte], offset: Int, appendLength: Int): Array[Byte] = {
    val newLength = offset + appendLength

    val current = bytes.length
    if(newLength > current) {
      if(newLength < 256) {
        resize(bytes, 256)
      }
      val num = if(newLength < current * 2) current * 2 else newLength
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

  def writeInt(bytes: Array[Byte], offset: Int, value: Int): Array[Byte] = {
    val bs = ensureCapacity(bytes, offset, 4)
    bs(offset) = value.asInstanceOf[Byte]
    bs(offset + 1) = (value >>> 8).asInstanceOf[Byte]
    bs(offset + 2) = (value >>> 16).asInstanceOf[Byte]
    bs(offset + 3) = (value >>> 24).asInstanceOf[Byte]
    bs
  }

  def writeLong(bytes: Array[Byte], offset: Int, value: Long): Array[Byte] = {
    val bs = ensureCapacity(bytes, offset, 8)
    bs(offset) = value.asInstanceOf[Byte]
    bs(offset + 1) = (value >>> 8).asInstanceOf[Byte]
    bs(offset + 2) = (value >>> 16).asInstanceOf[Byte]
    bs(offset + 3) = (value >>> 24).asInstanceOf[Byte]
    bs(offset + 4) = (value >>> 32).asInstanceOf[Byte]
    bs(offset + 5) = (value >>> 40).asInstanceOf[Byte]
    bs(offset + 6) = (value >>> 48).asInstanceOf[Byte]
    bs(offset + 7) = (value >>> 56).asInstanceOf[Byte]
    bs
  }

  def writeChar(bytes: Array[Byte], offset: Int, value: Char): Array[Byte] = {
    val byteSize = 2
    val bs = ensureCapacity(bytes, offset, byteSize)
    val cs = allocate(byteSize).putChar(value).array()
    for (i <- 0 to 1) bytes(offset + i) = cs(i)
    bs
  }
}
