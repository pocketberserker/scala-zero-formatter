package zeroformatter

import _root_.scalaz._

package object scalaz {

  implicit val formatterInvariantFunctor: InvariantFunctor[Formatter] = new InvariantFunctor[Formatter] {
    override def xmap[A, B](fa: Formatter[A], f: A => B, g: B => A) = fa.xmap(f, g)
  }

  implicit def maybeFormatter[T: Formatter]: Formatter[Maybe[T]] =
    Formatter[Option[T]].xmap(o => Maybe.fromOption(o), _.toOption)

  implicit def ilistFormatter[A](implicit F: Formatter[A]): Formatter[IList[A]] = new Formatter[IList[A]] {
    override val length = None

    override def serialize(encoder: Encoder, offset: Int, value: IList[A]) =
      if(value == null) encoder.writeInt(offset, -1)
      else {
        val length = value.length
        val byteSize =
          F.length.fold(encoder.writeInt(offset, length))(
            l => {
              encoder.ensureCapacity(offset, 4 + l * length)
              encoder.writeIntUnsafe(offset, length)
            }
          )

        value.foldLeft(byteSize){ case (acc, v) =>
          acc + F.serialize(encoder, offset + acc, v)
        }
      }

    override def deserialize(decoder: Decoder) = {
      val length = intFormatter.deserialize(decoder)
      if(length == -1) null
      else if(length < -1) throw FormatException(decoder.offset, "Invalid List length.")
      else {
        var list: IList[A] = IList.empty
        var i = 0
        while(i < length) {
          list ::= F.deserialize(decoder)
          i += 1
        }
        list.reverse
      }
    }
  }
}
