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

  implicit def formatterGen[T: Formatter]: Gen[Formatter[T]] =
    Gen.value(Formatter[T])

  implicit def formatterEqual[T: Equal: Gen]: Equal[Formatter[T]] = new Equal[Formatter[T]] {
    override def equal(a1: Formatter[T], a2: Formatter[T]) = {
      val value = Gen[T].sample()
      val r1 = ZeroFormatter.serialize(value)(a1)
      val r2 = ZeroFormatter.serialize(value)(a2)
      Equal[Array[Byte]].equal(r1, r2)
    }
  }

  implicit def lazyResultGen[T: Gen]: Gen[LazyResult[T]] = for {
    v <- Gen[T]
    s <- Gen[Int]
  } yield LazyResult(v, s)

  implicit def lazyResultEqual[T: Equal]: Equal[LazyResult[T]] = new Equal[LazyResult[T]] {
    override def equal(a1: LazyResult[T], a2: LazyResult[T]) = {
      Equal[T].equal(a1.value, a2.value) && Equal[Int].equal(a1.byteSize, a2.byteSize)
    }
  }
}
