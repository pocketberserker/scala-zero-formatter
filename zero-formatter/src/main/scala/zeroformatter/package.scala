import java.nio.ByteBuffer
import shapeless._
import shapeless.ops.hlist._

package object zeroformatter extends FormatterInstances {

  import BinaryUtil._
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

      override def serialize(bytes: Array[Byte], offset: Int, value: A) = {
        // [byteSize:int(4)] + [lastIndex:int(4)] + [indexOffset...:int(4 * lastIndex)]
        val initbyteSize = 4 + 4 + ((lastIndex + 1) * 4)
        val ObjectSerializerResult(bs, _, byteSize) =
          serializer.serialize(ObjectSerializerResult(bytes, offset, initbyteSize), value)
        val result = writeIntUnsafe(bs, offset + 4, lastIndex)
        LazyResult(writeInt(result, offset, byteSize), byteSize)
      }

      override def deserialize(buf: ByteBuffer, offset: Int) = {
        val byteSize = intFormatter.deserialize(buf, offset).value
        if(byteSize == -1) LazyResult(null.asInstanceOf[A], byteSize)
        else if(byteSize < -1) throw FormatException(offset, "Invalid byte size.")
        else {
          val li = buf.getInt(offset + 4)
          val result = formattersWithIndex.foldRight(ReadObjectResult(buf, offset, li, HNil: HNil))(readObject)
          LazyResult(gen.from(result.value), byteSize)
        }
      }
    }
  }

  implicit def structOptionFormatter[A <: Struct : Formatter]: Formatter[Option[A]] =
    nullableFormatter[A]
}
