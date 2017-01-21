package zeroformatter

final case class LazyList[F[_], A: Formatter](private val list: Vector[F[A]]) {

  val toVector: Vector[F[A]] = list

  val toList: List[F[A]] = list.toList
}

object LazyList {

  implicit def lazyListFormatter[F[_], A: Formatter](implicit
    F: Formatter[F[A]]
  ): Formatter[LazyList[F, A]] = new Formatter[LazyList[F, A]] {

    override val length = None

    override def serialize(encoder: Encoder, offset: Int, value: LazyList[F, A]) = {
      F.length match {
        case Some(_) => Formatter[Vector[F[A]]].serialize(encoder, offset, value.toVector)
        case None =>
          val v = value.toVector
          val len = v.length
          var byteSize = 4 + 4 + 4 * len
          encoder.ensureCapacity(offset, byteSize)
          var i = 0
          while(i < len) {
            encoder.writeIntUnsafe(offset + 4 + 4 + 4 * i, offset + byteSize)
            byteSize += F.serialize(encoder, offset + byteSize, v(i))
            i += 1
          }
          encoder.writeIntUnsafe(offset, byteSize)
          encoder.writeIntUnsafe(offset + 4, len)
          byteSize
      }
    }

    override def deserialize(decoder: Decoder) =
      F.length match {
        case Some(s) =>
          val len = decoder.readInt()
          if(len == -1) null.asInstanceOf[LazyList[F, A]]
          else if(len < -1) throw new FormatException(decoder.offset, "Invalid length.")
          else {
            var i = 0
            val builder = Vector.newBuilder[F[A]]
            while(i < len) {
              builder += F.deserialize(decoder)
              i += 1
              decoder.offset += s
            }
            LazyList(builder.result())
          }
        case None =>
          val start = decoder.offset
          val bs = decoder.readInt()
          if(bs == -1) null.asInstanceOf[LazyList[F, A]]
          else if(bs < -1) throw new FormatException(decoder.offset, "Invalid byteSize.")
          else {
            val l = decoder.readInt()
            val s = decoder.offset
            val builder = Vector.newBuilder[F[A]]
            var i = 0
            while(i < l) {
              decoder.offset = decoder.readInt(s + i * 4)
              builder += F.deserialize(decoder)
              i += 1
            }
            decoder.offset = start + bs
            LazyList(builder.result())
          }
      }
  }
}
