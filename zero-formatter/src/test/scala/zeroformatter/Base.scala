package zeroformatter

import dog.{Dog, Assert}
import scalaprops._
import scalaz.Equal
import scalaz.std.anyVal._
import scalaz.std.list._
import spire.math._

abstract class Base extends Dog with Assert {

  implicit def propParam = Param.withCurrentTimeSeed()

  implicit def arrayEq[A](implicit A: Equal[A]): Equal[Array[A]] =
    Equal.equalBy(_.toList)

  implicit val ubyteEqual: Equal[UByte] = Equal.equalBy(_.signed)
  implicit val ushortEqual: Equal[UShort] = Equal.equalBy(_.signed)
  implicit val uintEqual: Equal[UInt] = Equal.equalBy(_.signed)
  implicit val ulongEqual: Equal[ULong] = Equal.equalBy(_.signed)
}
