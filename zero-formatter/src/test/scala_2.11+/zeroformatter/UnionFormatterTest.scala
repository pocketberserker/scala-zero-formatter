package zeroformatter

import Formatter._
import UnionFormatter._
import dog._
import scalaz.std.anyVal._

sealed trait TestADT extends Union[Int] with Product with Serializable
final case class Test0(@Index(0) a: Int) extends TestADT {
    override val key = 1
  }
final case class Test1(@Index(0) b: Int, @Index(1) c: Int) extends TestADT {
  override val key = 2
}

object UnionFormatterTest extends Base {

  val `serialize Union[Int, Test0]` = TestCase {
    val value: TestADT = Test0(1)
    val bytes =
      Array(
        0x18, 0x00, 0x00, 0x00,

        0x01, 0x00, 0x00, 0x00,

        0x10, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x14, 0x00, 0x00, 0x00,
        0x01, 0x00, 0x00, 0x00
      ).map(_.toByte)
    for {
      _ <- assert.eq(bytes, ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize Union[Int, Test0]` = TestCase {
    for {
      values <- `serialize Union[Int, Test0]`
      _ <- assert.equal(values._1, ZeroFormatter.deserialize[TestADT](values._2)).lift
    } yield ()
  }

  val `serialize Union[Int, Test1]` = TestCase {
    val value: TestADT = Test1(2, 3)
    val bytes =
      Array(
        0x20, 0x00, 0x00, 0x00,

        0x02, 0x00, 0x00, 0x00,

        0x18, 0x00, 0x00, 0x00,
        0x01, 0x00, 0x00, 0x00,
        0x18, 0x00, 0x00, 0x00,
        0x1c, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00,
        0x03, 0x00, 0x00, 0x00
      ).map(_.toByte)
    for {
      _ <- assert.eq(bytes, ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize Union[Int, Test1]` = TestCase {
    for {
      values <- `serialize Union[Int, Test1]`
      _ <- assert.equal(values._1, ZeroFormatter.deserialize[TestADT](values._2)).lift
    } yield ()
  }
}
