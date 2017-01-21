package zeroformatter
package unsafe

import java.nio.charset.StandardCharsets

final case class UnsafeDecoder(buf: Array[Byte], private val _offset: Int) extends Decoder(_offset) {

  import UnsafeDecoder._

  override val buffer = buf

  override def readByte(): Byte = {
    offset += 1
    UnsafeUtil.getByte(buf, arrayBaseOffset + offset - 1)
  }

  override def readByte(o: Int): Byte =
    UnsafeUtil.getByte(buf, arrayBaseOffset + o)

  override def readShort(): Short = {
    offset += 2
    readShort(offset - 2)
  }

  override def readShort(o: Int): Short =
    UnsafeUtil.getShort(buf, arrayBaseOffset + o)

  override def readInt(): Int = {
    offset += 4
    readInt(offset - 4)
  }

  override def readInt(o: Int): Int =
    UnsafeUtil.getInt(buf, arrayBaseOffset + o)

  override def readLong(): Long = {
    offset += 8
    readLong(offset - 8)
  }

  override def readLong(o: Int): Long =
    UnsafeUtil.getLong(buf, arrayBaseOffset + o)

  override def readFloat(): Float = {
    offset += 4
    UnsafeUtil.getFloat(buf, arrayBaseOffset + offset - 4)
  }

  override def readDouble(): Double = {
    offset += 8
    UnsafeUtil.getDouble(buf, arrayBaseOffset + offset - 8)
  }

  override def readChar(): Char = {
    offset += 2
    UnsafeUtil.getChar(buf, arrayBaseOffset + offset - 2)
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
