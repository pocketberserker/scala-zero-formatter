package zeroformatter

import _root_.scalaz._
import Formatter._

package object scalaz {

  implicit val formatterInvariantFunctor: InvariantFunctor[Formatter] = new InvariantFunctor[Formatter] {
    override def xmap[A, B](fa: Formatter[A], f: A => B, g: B => A) = fa.xmap(f, g)
  }

  implicit val lazyResultFunctor: Functor[LazyResult] = new Functor[LazyResult] {
    override def map[A, B](fa: LazyResult[A])(f: A => B) = fa.map(f)
  }

  implicit def maybeFormatter[T: Formatter]: Formatter[Maybe[T]] =
    Formatter[Option[T]].xmap(o => Maybe.fromOption(o), _.toOption)

  object ZeroFormatter {

    def serialize[T: Formatter](value: T): Throwable \/ Array[Byte] =
      \/.fromTryCatchNonFatal(zeroformatter.ZeroFormatter.serialize[T](value))

    def deserialize[T: Formatter](value: Array[Byte]): Throwable \/ T =
      \/.fromTryCatchNonFatal(zeroformatter.ZeroFormatter.deserialize[T](value))
  }
}
