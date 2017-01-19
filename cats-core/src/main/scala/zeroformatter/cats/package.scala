package zeroformatter

import _root_.cats._
import _root_.cats.functor._

package object cats {

  implicit val formatterInvariant: Invariant[Formatter] = new Invariant[Formatter] {
    def imap[A, B](fa: Formatter[A])(f: A => B)(g: B => A): Formatter[B] = fa.xmap(f, g)
  }

  implicit def evalFormatter[T](implicit F: Formatter[T]): Formatter[Eval[T]] = new Formatter[Eval[T]] {
    override def length = F.length
    override def serialize(encoder: Encoder, offset: Int, v: Eval[T]) =
      F.serialize(encoder, offset, v.value)
    override def deserialize(decoder: Decoder) = {
      val d = decoder.newOffset(decoder.offset)
      Eval.later(F.deserialize(d))
    }
  }

  implicit def lazyListFormatter[A: Formatter]: Formatter[LazyList[Eval, A]] =
    LazyList.lazyListFormatter
}
