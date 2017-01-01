package zeroformatter


import _root_.cats.Eval
import _root_.cats.functor._

package object cats {

  implicit val formatterInvariantInstance: Invariant[Formatter] = new Invariant[Formatter] {
    def imap[A, B](fa: Formatter[A])(f: A => B)(g: B => A): Formatter[B] = fa.xmap(f, g)
  }

  object ZeroFormatter {

    def serialize[T: Formatter](value: T): Array[Byte] =
      zeroformatter.ZeroFormatter.serialize[T](value)

    def deserialize[T: Formatter](bytes: Array[Byte]): Eval[T] =
      Eval.later(zeroformatter.ZeroFormatter.deserialize[T](bytes))
  }
}
