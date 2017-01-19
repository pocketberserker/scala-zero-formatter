package zeroformatter

import shapeless._
import shapeless.ops.hlist._

abstract class Enum[
  @specialized(Byte, Short, Int, Long)T: Formatter
] extends ZeroFormattable {

  def label: T

  private[this] val formatter = implicitly[Formatter[T]]

  override val length: Option[Int] = formatter.length

  def serialize(encoder: Encoder, offset: Int): Int =
    formatter.serialize(encoder, offset, label)

  def check(decoder: Decoder): Boolean = {
    val o = decoder.offset
    val r = formatter.deserialize(decoder)
    if(r == label) true
    else {
      decoder.offset = o
      false
    }
  }
}

object Enum {

  private[this] object writeEnum extends Poly2 {
    implicit def default[T <: Enum[_]] =
      at[ObjectSerializerResult, T] {
        case (acc, value) =>
          acc.copy(byteSize = acc.byteSize + value.serialize(acc.encoder, acc.offset))
      }
  }

  private[this] case class EnumFormatterResult[C <: Coproduct, H <: HList](fake: C, value: H)

  private[this] object genEnumFormatter extends Poly2 {
    implicit def gen[
      N <: Nat, C <: Coproduct, H <: HList, V <: Enum[_],
      K <: HList
    ](implicit
      a: ops.coproduct.At.Aux[C, N, V],
      gen: Generic.Aux[V, K],
      init: FillWith[zero.type, K]
    ) =
      at[N, EnumFormatterResult[C, H]]{ case (_, acc) =>
        val kf = gen.from(HList.fillWith[K](zero))
        EnumFormatterResult(acc.fake, kf :: acc.value)
      }
  }

  private[this] case class ReadEnumResult[T](decoder: Decoder, value: Option[T])

  private[this] object readEnum extends Poly2 {
    implicit def read[V <: Enum[_], U <: Enum[_]] =
      at[ReadEnumResult[U], V] {
        case (acc, f) => acc.value match {
          case Some(_) => acc
          case None if f.check(acc.decoder) =>
            acc.copy(value = Some(f.asInstanceOf[U]))
          case None => acc
        }
      }
  }

  implicit def enumFormatter[
    A <: Enum[_], B <: Coproduct,
    C <: Nat, D <: HList, E <: HList, F <: Enum[_], G <: HList
  ](implicit
    gen: Generic.Aux[A, B],
    length: ops.coproduct.Length.Aux[B, C],
    range: ops.nat.Range.Aux[nat._0, C, D],
    generator: RightFolder.Aux[D, EnumFormatterResult[B, HNil], genEnumFormatter.type, EnumFormatterResult[B, E]],
    isHCons: IsHCons.Aux[E, F, G],
    // serialize
    write: ops.coproduct.LeftFolder.Aux[B, ObjectSerializerResult, writeEnum.type, ObjectSerializerResult],
    // deserialize
    read: LeftFolder.Aux[E, ReadEnumResult[A], readEnum.type, ReadEnumResult[A]]
  ): Formatter[A] = {

    val fs =
      range()
        .foldRight(EnumFormatterResult(null.asInstanceOf[B], HNil: HNil))(genEnumFormatter)
        .value

    new Formatter[A] {

      override val length = fs.head.length

      override def serialize(encoder: Encoder, offset: Int, value: A) = {
        val values = gen.to(value)
        values.foldLeft(ObjectSerializerResult(encoder, offset, 0))(writeEnum)
          .byteSize
      }

      override def deserialize(decoder: Decoder) = {
        fs.foldLeft(ReadEnumResult(decoder, None: Option[A]))(readEnum)
          .value
          .getOrElse(throw FormatException(decoder.offset, "EnumFormatter could not deserialize Enum label."))
      }
    }
  }
}
