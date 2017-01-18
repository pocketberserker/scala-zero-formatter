package zeroformatter

import _root_.scalaz._

package object scalaz {

  implicit val formatterInvariantFunctor: InvariantFunctor[Formatter] = new InvariantFunctor[Formatter] {
    override def xmap[A, B](fa: Formatter[A], f: A => B, g: B => A) = fa.xmap(f, g)
  }

  implicit val lazyResultFunctor: Functor[LazyResult] = new Functor[LazyResult] {
    override def map[A, B](fa: LazyResult[A])(f: A => B) = fa.map(a => f(a))
  }

  implicit def maybeFormatter[T: Formatter]: Formatter[Maybe[T]] =
    Formatter[Option[T]].xmap(o => Maybe.fromOption(o), _.toOption)

  implicit def ilistFormatter[A](implicit F: Formatter[A]): Formatter[IList[A]] = new Formatter[IList[A]] {
    override val length = None

    override def serialize(bytes: Array[Byte], offset: Int, value: IList[A]) =
      if(value == null) intFormatter.serialize(bytes, offset, -1)
      else {
        val length = value.length
        val bs =
          F.length.map(l => BinaryUtil.ensureCapacity(bytes, offset, 4 + l * length))
            .getOrElse(bytes)

        value.foldLeft(intFormatter.serialize(bs, offset, length)){ case (acc, v) =>
          val r = F.serialize(acc.value, offset + acc.byteSize, v)
          LazyResult.strict(r.value, acc.byteSize + r.byteSize)
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
