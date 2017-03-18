package zeroformatter
package zstd

import com.github.luben.zstd.Zstd

object ZeroFormatter {

  def serialize[T](value: T, level: Int = 3)(implicit F: Formatter[T]): Array[Byte] = {
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
      //val maxCompressedLength = Zstd.compressBound(decompressedLength).toInt
      //val compressed = new Array[Byte](maxCompressedLength + 4)
      //val size = Zstd.compressFastDict(compressed, 4, binary, 0, decompressedLength, null)
      //if(Zstd.isError(size)) throw new Exception(Zstd.getErrorName(size))
      //else {
      //  encoder = unsafe.UnsafeEncoder(compressed)
      //  encoder.writeIntUnsafe(0, decompressedLength)
      //  encoder.resize(size.toInt)
      //  encoder.toByteArray
      //}
      encoder.resize(decompressedLength)
      val compressed = Zstd.compress(encoder.toByteArray, level)
      encoder = unsafe.UnsafeEncoder(new Array[Byte](compressed.length + 4))
      encoder.writeIntUnsafe(0, decompressedLength)
      encoder.writeByteArrayUnsafe(4, compressed)
      encoder.toByteArray
    }
  }

  def deserialize[T](bytes: Array[Byte])(implicit F: Formatter[T]): T = {
    val decoder = unsafe.UnsafeDecoder(bytes, 0)
    val decompressedLength = decoder.readInt()
    if(decompressedLength < 0) throw FormatException(0, s"Invalid lz4 decompressed length($decompressedLength).")
    else if(decompressedLength <= 64) F.deserialize(decoder)
    else {
      //val restored = new Array[Byte](decompressedLength)
      //val size = Zstd.decompressFastDict(restored, 0, bytes, 4, bytes.length - 4, null)
      //if(Zstd.isError(size)) throw new FormatException(4, Zstd.getErrorName(size))
      //else unsafe.ZeroFormatter.deserialize[T](restored)
      val encoder = unsafe.UnsafeEncoder(new Array[Byte](bytes.length - 4))
      encoder.writeByteArrayUnsafe(0, bytes, 4, bytes.length - 4)
      unsafe.ZeroFormatter.deserialize[T](Zstd.decompress(encoder.toByteArray, decompressedLength))
    }
  }
}
