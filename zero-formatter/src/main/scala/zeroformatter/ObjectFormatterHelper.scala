package zeroformatter

import java.nio.ByteBuffer
import shapeless._

private[zeroformatter] object zero extends Poly0 {
  implicit def zero[T] = at[T](null.asInstanceOf[T])
}

private[zeroformatter] object ObjectFormatterHelper {

  object flatten extends Poly1 {
    implicit def some[T] = at[Some[Index]]{
      case Some(index) => index.value
    }
  }

  object genObjectFormatter extends Poly2 {
    implicit def gen[T, U <: HList](implicit F: Formatter[T]) =
      at[T, U]((_, acc) => F :: acc)
  }

  case class ReadObjectResult[T <: HList](buf: ByteBuffer, offset: Int, lastIndex: Int, value: T)

  object readObject extends Poly2 {
    implicit def read[T, U <: HList] =
      at[(Formatter[T], Int), ReadObjectResult[U]] {
        case ((formatter, index), acc) =>
          val v =
            if(index > acc.lastIndex) formatter.default
            else {
              val o = intFormatter.deserialize(acc.buf, acc.offset + 4 + 4 + 4 * index).value
              if(o == 0) formatter.default
              else formatter.deserialize(acc.buf, o).value
            }
          acc.copy(value = v :: acc.value)
      }
  }
}
