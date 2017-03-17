package zeroformatter
package unsafe

final case class UnsafeEncoder(private var buf: Array[Byte]) extends Encoder {

  import UnsafeEncoder._

  override def ensureCapacity(offset: Int, appendLength: Int): Unit = {
    buf = UnsafeUtil.ensureCapacity(buf, offset, appendLength)
  }

  override def toByteArray = buf

  override def writeBoolUnsafe(offset: Int, value: Boolean): Int = {
    UnsafeUtil.putBoolean(buf, arrayBaseOffset + offset, value)
    1
  }

  override def writeBool(offset: Int, value: Boolean): Int = {
    ensureCapacity(offset, 1)
    writeBoolUnsafe(offset, value)
  }

  override def writeByte(offset: Int, value: Byte): Int = {
    ensureCapacity(offset, 1)
    UnsafeUtil.putByte(buf, arrayBaseOffset + offset, value)
    1
  }

  override def writeByteUnsafe(offset: Int, value: Byte): Int = {
    UnsafeUtil.putByte(buf, arrayBaseOffset + offset, value)
    1
  }

  override def writeShortUnsafe(offset: Int, value: Short): Int = {
    UnsafeUtil.putShort(buf, arrayBaseOffset + offset, value)
    2
  }

  override def writeShort(offset: Int, value: Short): Int = {
    ensureCapacity(offset, 2)
    writeShortUnsafe(offset, value)
  }

  override def writeIntUnsafe(offset: Int, value: Int): Int = {
    UnsafeUtil.putInt(buf, arrayBaseOffset + offset, value)
    4
  }

  override def writeInt(offset: Int, value: Int): Int = {
    ensureCapacity(offset, 4)
    writeIntUnsafe(offset, value)
  }

  override def writeLongUnsafe(offset: Int, value: Long): Int = {
    UnsafeUtil.putLong(buf, arrayBaseOffset + offset, value)
    8
  }

  override def writeLong(offset: Int, value: Long): Int = {
    ensureCapacity(offset, 8)
    writeLongUnsafe(offset, value)
  }

  override def writeFloatUnsafe(offset: Int, value: Float): Int = {
    UnsafeUtil.putFloat(buf, arrayBaseOffset + offset, value)
    4
  }

  override def writeFloat(offset: Int, value: Float): Int = {
    ensureCapacity(offset, 4)
    writeFloatUnsafe(offset, value)
  }

  override def writeDoubleUnsafe(offset: Int, value: Double): Int = {
    UnsafeUtil.putDouble(buf, arrayBaseOffset + offset, value)
    8
  }

  override def writeDouble(offset: Int, value: Double): Int = {
    ensureCapacity(offset, 8)
    writeDoubleUnsafe(offset, value)
  }

  override def writeCharUnsafe(offset: Int, value: Char): Int = {
    UnsafeUtil.putChar(buf, arrayBaseOffset + offset, value)
    2
  }

  override def writeChar(offset: Int, value: Char): Int = {
    ensureCapacity(offset, 2)
    writeCharUnsafe(offset, value)
  }

  override def writeByteArrayUnsafe(offset: Int, value: Array[Byte]): Int =
    writeByteArrayUnsafe(offset, value, 0, value.length)

  override def writeByteArrayUnsafe(offset: Int, value: Array[Byte], start: Int, len: Int) = {
    UnsafeUtil.copyMemory(value, arrayBaseOffset + start, buf, arrayBaseOffset + offset, len)
    len
  }

  override def writeByteArray(offset: Int, value: Array[Byte]): Int =
    writeByteArray(offset, value, 0, value.length)

  override def writeByteArray(offset: Int, value: Array[Byte], start: Int, len: Int) = {
    ensureCapacity(offset, len)
    writeByteArrayUnsafe(offset, value, start, len)
  }

  override def writeString(offset: Int, value: String): Int =
    if(value == null) writeInt(offset, -1)
    else {
      val len = Utf8.encodedLength(value)
      ensureCapacity(offset, 4 + len)
      val intSize = writeIntUnsafe(offset, len)
      Utf8.encode(value, buf, offset + intSize, len)
      intSize + len
    }
}

private[unsafe] object UnsafeEncoder {

  private[unsafe] val arrayBaseOffset: Long = UnsafeUtil.getArrayBaseOffset()
}
