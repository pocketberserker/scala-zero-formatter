package zeroformatter

import _root_.scalaz._
import Formatter._

package object scalaz {

  implicit val formatterInvariantFunctorInstance: InvariantFunctor[Formatter] = new InvariantFunctor[Formatter] {
    def xmap[A, B](fa: Formatter[A], f: A => B, g: B => A): Formatter[B] = fa.xmap(f, g)
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
