package zeroformatter

import shapeless._
import shapeless.ops.hlist._
import FormatterHelper._

abstract class Union[K: Formatter] {

  def key: K

  def serializeKey(encoder: Encoder, offset: Int): Int =
    implicitly[Formatter[K]].serialize(encoder, offset, key)

  def checkKey(decoder: Decoder): Boolean = {
    val o = decoder.offset
    val r = implicitly[Formatter[K]].deserialize(decoder)
    if(r == key) true
    else {
      decoder.offset = o
      false
    }
  }
}

object Union {

  private[this] object writeUnion extends Poly2 {
    implicit def default[T <: Union[_]](implicit F: Formatter[T]) =
      at[ObjectSerializerResult, T] {
        case (ObjectSerializerResult(encoder, offset, byteSize), value) =>
          val v = value.serializeKey(encoder, offset)
          val result = F.serialize(encoder, offset + v, value)
          ObjectSerializerResult(encoder, offset + v + result, byteSize + v + result)
      }
  }

  private[this] case class UnionFormatterResult[C <: Coproduct, H <: HList](fake: C, value: H)

  private[this] object genUnionFormatter extends Poly2 {
    implicit def gen[
      N <: Nat, C <: Coproduct, H <: HList, V <: Union[_],
      K <: HList
    ](implicit
      a: ops.coproduct.At.Aux[C, N, V],
      V: Formatter[V],
      gen: Generic.Aux[V, K],
      init: FillWith[zero.type, K]
    ) =
      at[N, UnionFormatterResult[C, H]]{ case (_, acc) =>
        val kf = gen.from(HList.fillWith[K](zero))
        UnionFormatterResult(acc.fake, (kf, V) :: acc.value)
      }
  }

  private[this] case class ReadUnionResult[T](decoder: Decoder, value: Option[T])

  private[this] object readUnion extends Poly2 {
    implicit def read[V <: Union[_], U <: Union[_]] =
      at[ReadUnionResult[U], (V, Formatter[V])] {
        case (acc, (kf, vf)) => acc.value match {
          case Some(_) => acc
          case None if kf.checkKey(acc.decoder) =>
            val v = vf.deserialize(acc.decoder)
            acc.copy(value = Some(v.asInstanceOf[U]))
          case None => acc
        }
      }
  }

  implicit def unionFormatter[
    A <: Union[_],
    B <: Coproduct,
    C <: Nat, D <: HList, E <: HList
  ](implicit
    gen: Generic.Aux[A, B],
    // serialize
    write: ops.coproduct.LeftFolder.Aux[B, ObjectSerializerResult, writeUnion.type, ObjectSerializerResult],
    // deserialize
    length: ops.coproduct.Length.Aux[B, C],
    range: ops.nat.Range.Aux[nat._0, C, D],
    generator: RightFolder.Aux[D, UnionFormatterResult[B, HNil], genUnionFormatter.type, UnionFormatterResult[B, E]],
    read: LeftFolder.Aux[E, ReadUnionResult[A], readUnion.type, ReadUnionResult[A]]
  ): Formatter[A] = {

    val fs =
      range()
        .foldRight(UnionFormatterResult(null.asInstanceOf[B], HNil: HNil))(genUnionFormatter)
        .value

    new Formatter[A] {

      override val length = None

      override def serialize(encoder: Encoder, offset: Int, value: A) = {
        val values = gen.to(value)
        val ObjectSerializerResult(_, _, byteSize) =
          values.foldLeft(ObjectSerializerResult(encoder, offset + 4, 4))(writeUnion)
        encoder.writeIntUnsafe(offset, byteSize)
        byteSize
      }

      override def deserialize(decoder: Decoder) = {
        val byteSize = decoder.readInt()
        if(byteSize == -1) null.asInstanceOf[A]
        else if(byteSize < -1) throw FormatException(decoder.offset - 4, s"Invalid byte size($byteSize).")
        else {
          val result = fs.foldLeft(ReadUnionResult(decoder, None: Option[A]))(readUnion)
          result.value.getOrElse(throw FormatException(decoder.offset, "Union key does not find."))
        }
      }
    }
  }
}
