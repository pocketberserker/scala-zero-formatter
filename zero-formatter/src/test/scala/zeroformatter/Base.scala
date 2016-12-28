package zeroformatter

import dog.{Dog, Assert}
import scalaprops._
import scalaz.Equal
import scalaz.std.list._

abstract class Base extends Dog with Assert {

  implicit def propParam = Param.withCurrentTimeSeed()

  implicit def arrayEq[A](implicit A: Equal[A]): Equal[Array[A]] =
    Equal.equalBy(_.toList)
}
