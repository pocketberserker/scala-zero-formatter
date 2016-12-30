package zeroformatter

import java.nio.ByteBuffer

abstract class Union[K: Formatter] {

  def key: K

  def serializeKey(bytes: Array[Byte], offset: Int): (Array[Byte], Int) =
    implicitly[Formatter[K]].serialize(bytes, offset, key)

  def checkKey(buf: ByteBuffer, offset: Int): Option[Int] = {
    val r = implicitly[Formatter[K]].deserialize(buf, offset)
    if(r.value == key) Some(r.byteSize)
    else None
  }
}
