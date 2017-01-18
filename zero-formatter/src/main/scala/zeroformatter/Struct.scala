package zeroformatter

import shapeless._
import shapeless.ops.hlist._
import ObjectFormatterHelper._

trait Struct

object Struct {

  private[this] case class ReadStructResult[T <: HList](decoder: Decoder, value: T)

  private[this] object readStruct extends Poly2 {
    implicit def read[T, U <: HList] =
      at[ReadStructResult[U], Formatter[T]] {
        case (acc, formatter) =>
          acc.copy(value = formatter.deserialize(acc.decoder) :: acc.value)
      }
  }

  implicit def structFormatter[
    A <: Struct, B <: HList, C <: HList, D <: HList
  ](implicit
    gen: Generic.Aux[A, B],
    // serialize
    serializer: ObjectSerializer[A],
    // deserialize
    init: FillWith[zero.type, B],
    generator: RightFolder.Aux[B, HNil, genObjectFormatter.type, C],
    read: LeftFolder.Aux[C, ReadStructResult[HNil], readStruct.type, ReadStructResult[D]],
    reverse: Reverse.Aux[D, B]
    ): Formatter[A] = {

    val formatters =
      HList.fillWith[B](zero)
        .foldRight(HNil: HNil)(genObjectFormatter)

    new Formatter[A] {

      override val length = None

      override def serialize(bytes: Array[Byte], offset: Int, value: A) = {
        val ObjectSerializerResult(result, _, byteSize) =
          serializer.serialize(ObjectSerializerResult(bytes, offset, 0), value)
        LazyResult(result, byteSize)
      }

      override def deserialize(decoder: Decoder) =
        gen.from(formatters.foldLeft(ReadStructResult(decoder, HNil: HNil))(readStruct).value.reverse)
    }
  }
}
