package zeroformatter

import Formatter._
import dog._
import scalaz.std.anyVal._

object StructFormatterTest extends Base {

  case class MyClass(
    age: Int,
    firstName: String,
    laftName: String
  ) extends Struct

  val `serialize Struct` = TestCase {
    val value = MyClass(99, "hoge", "fuga")
    val bytes =
      Array(
        0x63, 0x00, 0x00, 0x00,
        0x04, 0x00, 0x00, 0x00,
        0x68, 0x6f, 0x67, 0x65,
        0x04, 0x00, 0x00, 0x00,
        0x66, 0x75, 0x67, 0x61
      ).map(_.toByte)
    for {
      _ <- assert.eq(bytes, ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize Struct` = TestCase {
    for {
      values <- `serialize Struct`
      _ <- assert.equal(values._1, ZeroFormatter.deserialize[MyClass](values._2)).lift
    } yield ()
  }

  val `serialize Option[Struct]` = TestCase {
    val value: Option[MyClass] = Some(MyClass(99, "hoge", "fuga"))
    val bytes =
      Array(
        0x01,
        0x63, 0x00, 0x00, 0x00,
        0x04, 0x00, 0x00, 0x00,
        0x68, 0x6f, 0x67, 0x65,
        0x04, 0x00, 0x00, 0x00,
        0x66, 0x75, 0x67, 0x61
      ).map(_.toByte)
    for {
      _ <- assert.eq(bytes, ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize Option[Struct]` = TestCase {
    for {
      values <- `serialize Option[Struct]`
      _ <- assert.equal(values._1, ZeroFormatter.deserialize[Option[MyClass]](values._2)).lift
    } yield ()
  }
}
