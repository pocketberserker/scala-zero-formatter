import shapeless._
import shapeless.ops.hlist._

package object zeroformatter extends FormatterInstances {

  import ObjectFormatterHelper._

  implicit def objectFormatter[
    A, B <: HList, C <: HList, D <: HList, E <: HList, F <: HList,
    G <: HList, H <: HList
  ](implicit
    gen: Generic.Aux[A, B],
    index: Annotations.Aux[Index, A, C],
    filterIndex: Partition.Aux[C, None.type, D, E],
    flattener: Mapper.Aux[flatten.type, E, F],
    toIndexList: ToTraversable.Aux[F, List, Int],
    // serialize
    serializer: ObjectSerializer[A],
    // deserialize
    init: FillWith[zero.type, B],
    generator: RightFolder.Aux[B, HNil, genObjectFormatter.type, G],
    formatterZipper : Zip.Aux[G :: F :: HNil, H],
    read: RightFolder.Aux[H, ReadObjectResult[HNil], readObject.type, ReadObjectResult[B]]
    ): Formatter[A] = {

    val indexes = Annotations[Index, A].apply().filterNot[None.type].map(flatten)
    val lastIndex = indexes.toList.reduceOption(_ max _).getOrElse(throw FormatException(0, "Target object does not have index fields."))
    val formattersWithIndex =
      HList.fillWith[B](zero)
        .foldRight(HNil: HNil)(genObjectFormatter)
        .zip(indexes)

    new Formatter[A] {

      override val length = None

      override def serialize(encoder: Encoder, offset: Int, value: A) = {
        // [byteSize:int(4)] + [lastIndex:int(4)] + [indexOffset...:int(4 * lastIndex)]
        val initbyteSize = 4 + 4 + ((lastIndex + 1) * 4)
        val byteSize = serializer.serialize(encoder, offset, initbyteSize, value)
        encoder.writeIntUnsafe(offset + 4, lastIndex)
        encoder.writeIntUnsafe(offset, byteSize)
        byteSize
      }

      override def deserialize(decoder: Decoder) = {
        val byteSize = decoder.getInt()
        if(byteSize == -1) null.asInstanceOf[A]
        else if(byteSize < -1) throw FormatException(decoder.offset, "Invalid byte size.")
        else {
          val o = decoder.offset - 4
          val li = decoder.getInt()
          val result = formattersWithIndex.foldRight(ReadObjectResult(decoder, o, li, HNil: HNil))(readObject)
          decoder.offset = o + byteSize
          gen.from(result.value)
        }
      }
    }
  }

  implicit def structOptionFormatter[A <: Struct : Formatter]: Formatter[Option[A]] =
    nullableFormatter[A]
}
