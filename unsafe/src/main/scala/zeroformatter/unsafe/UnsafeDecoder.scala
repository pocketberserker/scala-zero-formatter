package zeroformatter
package unsafe

import java.nio.charset.StandardCharsets

final case class UnsafeDecoder(buf: Array[Byte], private val _offset: Int) extends Decoder(_offset) {

  import UnsafeDecoder._

  override val buffer = buf

  override def readByte(): Byte = {
    val r = UnsafeUtil.getByte(buf, arrayBaseOffset + offset)
    offset += 1
    r
  }

  override def readByte(o: Int): Byte =
    UnsafeUtil.getByte(buf, arrayBaseOffset + o)

  override def readShort(o: Int): Short =
    UnsafeUtil.getShort(buf, arrayBaseOffset + o)

  override def readInt(o: Int): Int =
    UnsafeUtil.getInt(buf, arrayBaseOffset + o)

  override def readLong(o: Int): Long =
    UnsafeUtil.getLong(buf, arrayBaseOffset + o)

  override def readFloat(): Float = {
    val r = UnsafeUtil.getFloat(buf, arrayBaseOffset + offset)
    offset += 4
    r
  }

  override def readDouble(): Double = {
    val r = UnsafeUtil.getDouble(buf, arrayBaseOffset + offset)
    offset += 8
    r
  }

  override def readChar(): Char = {
    val r = UnsafeUtil.getChar(buf, arrayBaseOffset + offset)
    offset += 2
    r
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

private[unsafe] object UnsafeDecoder {

  private[unsafe] val arrayBaseOffset: Long = UnsafeUtil.getArrayBaseOffset()
}
