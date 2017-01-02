package zeroformatter

import java.nio.ByteBuffer
import _root_.cats._
import _root_.cats.functor._

package object cats {

  implicit val formatterInvariant: Invariant[Formatter] = new Invariant[Formatter] {
    def imap[A, B](fa: Formatter[A])(f: A => B)(g: B => A): Formatter[B] = fa.xmap(f, g)
  }

  implicit val lazyResultFunctor: Functor[LazyResult] = new Functor[LazyResult] {
    override def map[A, B](fa: LazyResult[A])(f: A => B) = fa.map(a => f(a))
  }

  implicit def evalFormatter[T](implicit F: Formatter[T]): Formatter[Eval[T]] = new Formatter[Eval[T]] {
    override def length = F.length
    override def serialize(bytes: Array[Byte], offset: Int, v: Eval[T]) =
      F.serialize(bytes, offset, v.value)
    override def deserialize(buf: ByteBuffer, offset: Int) =
      F.deserialize(buf, offset).map(v => Eval.later(v))
  }

  object ZeroFormatter {

    def serialize[T: Formatter](value: T): Array[Byte] =
      zeroformatter.ZeroFormatter.serialize[T](value)

    def deserialize[T: Formatter](bytes: Array[Byte]): Eval[T] =
      Eval.later(zeroformatter.ZeroFormatter.deserialize[T](bytes))
  }
}
