package zeroformatter
package lz4

import net.jpountz.lz4._

object ZeroFormatter {

  private[this] val factory = LZ4Factory.fastestInstance()

  def serialize[T](value: T)(implicit F: Formatter[T]): Array[Byte] = {
    var encoder = unsafe.UnsafeEncoder(new Array[Byte](F.length.getOrElse(0)))
    val decompressedLength = F.serialize(encoder, 0, value)
    val binary = encoder.toByteArray
    if(decompressedLength <= 64) {
      encoder = unsafe.UnsafeEncoder(new Array[Byte](decompressedLength + 4))
      encoder.writeIntUnsafe(0, decompressedLength)
      encoder.writeByteArrayUnsafe(4, binary, 0, decompressedLength)
      encoder.toByteArray
    }
    else {
      val compressor = factory.fastCompressor()
      val maxCompressedLength = compressor.maxCompressedLength(decompressedLength)
      val compressed = new Array[Byte](maxCompressedLength + 4)
      compressor.compress(binary, 0, decompressedLength, compressed, 4, maxCompressedLength)
      encoder = unsafe.UnsafeEncoder(compressed)
      encoder.writeIntUnsafe(0, decompressedLength)
      encoder.toByteArray
    }
  }

  def deserialize[T](bytes: Array[Byte])(implicit F: Formatter[T]): T = {
    val decoder = unsafe.UnsafeDecoder(bytes, 0)
    val decompressedLength = decoder.readInt()
    if(decompressedLength < 0) throw FormatException(0, s"Invalid lz4 decompressed length($decompressedLength).")
    else if(decompressedLength <= 64) F.deserialize(decoder)
    else {
      val decompressor = factory.fastDecompressor()
      val restored = new Array[Byte](decompressedLength)
      decompressor.decompress(bytes, 4, restored, 0, decompressedLength)
      unsafe.ZeroFormatter.deserialize(restored)
    }
  }
}
