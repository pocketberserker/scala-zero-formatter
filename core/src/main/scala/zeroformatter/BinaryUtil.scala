package zeroformatter

import java.nio.{ByteBuffer, ByteOrder}

private[zeroformatter] object BinaryUtil {

  def wrapByteArray(bytes: Array[Byte]): ByteBuffer =
    ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)

  def allocate(capacity: Int): ByteBuffer =
    ByteBuffer.allocate(capacity).order(ByteOrder.LITTLE_ENDIAN)

  def resize(array: Array[Byte], newSize: Int): Array[Byte] = {
    if(array.length != newSize) {
      val array2 = new Array[Byte](newSize)
      val l = array.length
      array.copyToArray(array2, 0, if(l > newSize) newSize else l)
      array2
    }
    else array
  }

  def ensureCapacity(bytes: Array[Byte], offset: Int, appendLength: Int): Array[Byte] = {
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
}
