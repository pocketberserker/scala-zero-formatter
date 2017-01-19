package zeroformatter

import shapeless._

private[zeroformatter] object FormatterHelper {

  case class ObjectSerializerResult(encoder: Encoder, offset: Int, byteSize: Int)

  object genObjectFormatter extends Poly2 {
    implicit def gen[T, U <: HList](implicit F: Formatter[T]) =
      at[T, U]((_, acc) => F :: acc)
  }

  object zero extends Poly0 {
    implicit def zero[T] = at[T](null.asInstanceOf[T])
  }
}
