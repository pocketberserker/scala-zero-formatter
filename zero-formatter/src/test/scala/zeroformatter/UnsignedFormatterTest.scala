package zeroformatter

import spire.math._
import Formatter._
import dog._
import scalaz.std.anyVal._
import scalaz.std.option._

object UnsignedFormatterTest extends Base {

  val `serialize UByte` = TestCase {
    val value = UByte.MaxValue
    val bytes = Array(0xff.toByte)
    for {
      _ <- assert.eq(bytes, ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize Byte` = TestCase {
    for {
      values <- `serialize UByte`
      _ <- assert.eq(values._1, ZeroFormatter.deserialize[UByte](values._2)).lift
    } yield ()
  }

  val `serialize Option[UByte]` = TestCase {
    val value: Option[UByte] = Some(UByte(123.toByte))
    val bytes = Array(0x01.toByte, 0x7b.toByte)
    for {
      _ <- assert.eq(bytes, ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize Option[UByte]` = TestCase {
    for {
      values <- `serialize Option[UByte]`
      _ <- assert.eq(values._1, ZeroFormatter.deserialize[Option[UByte]](values._2)).lift
    } yield ()
  }

  val `serialize UShort` = TestCase {
    val value = UShort.MaxValue
    val bytes = Array(0xff, 0xff).map(_.toByte)
    for {
      _ <- assert.eq(bytes, ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize UShort` = TestCase {
    for {
      values <- `serialize UShort`
      _ <- assert.eq(values._1, ZeroFormatter.deserialize[UShort](values._2)).lift
    } yield ()
  }

  val `serialize UInt` = TestCase {
    val value = UInt.MaxValue
    val bytes = Array(0xff, 0xff, 0xff, 0xff).map(_.toByte)
    for {
      _ <- assert.eq(bytes, ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize UInt` = TestCase {
    for {
      values <- `serialize UInt`
      _ <- assert.eq(values._1, ZeroFormatter.deserialize[UInt](values._2)).lift
    } yield ()
  }

  val `serialize ULong` = TestCase {
    val value = ULong.MaxValue
    val bytes = Array(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff).map(_.toByte)
    for {
      _ <- assert.eq(bytes, ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize ULong` = TestCase {
    for {
      values <- `serialize ULong`
      _ <- assert.eq(values._1, ZeroFormatter.deserialize[ULong](values._2)).lift
    } yield ()
  }
}
