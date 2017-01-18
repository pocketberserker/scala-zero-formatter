package zeroformatter

import shapeless._
import shapeless.ops.hlist._

abstract class Enum[
  @specialized(Byte, Short, Int, Long)T: Formatter
] extends ZeroFormattable {

  def label: T

  private[this] val formatter = implicitly[Formatter[T]]

  override val length: Option[Int] = formatter.length

  def serialize(bytes: Array[Byte], offset: Int): LazyResult[Array[Byte]] =
    formatter.serialize(bytes, offset, label)

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
      at[(LazyResult[Array[Byte]], Int), T] {
        case ((result, offset), value) =>
          (value.serialize(result.value, offset), offset)
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
    write: ops.coproduct.LeftFolder.Aux[B, (LazyResult[Array[Byte]], Int), writeEnum.type, (LazyResult[Array[Byte]], Int)],
    // deserialize
    read: LeftFolder.Aux[E, ReadEnumResult[A], readEnum.type, ReadEnumResult[A]]
  ): Formatter[A] = {

    val fs =
      range()
        .foldRight(EnumFormatterResult(null.asInstanceOf[B], HNil: HNil))(genEnumFormatter)
        .value

    new Formatter[A] {

      override val length = fs.head.length

      override def serialize(bytes: Array[Byte], offset: Int, value: A) = {
        val values = gen.to(value)
        values.foldLeft((LazyResult.strict(bytes, 0), offset))(writeEnum)
          ._1
      }

      override def deserialize(decoder: Decoder) = {
        fs.foldLeft(ReadEnumResult(decoder, None: Option[A]))(readEnum)
          .value
          .getOrElse(throw FormatException(decoder.offset, "EnumFormatter could not deserialize Enum label."))
      }
    }
  }
}
