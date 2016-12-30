package zeroformatter

import Formatter._
import dog._
import scalaz.Equal
import scalaz.std.anyVal._
import scalaz.std.string._

object ArrayFormatterTest extends Base {

  val `serialize Array[Int]` = TestCase {
    val value = Array(0, 1, 2, 3)
    val bytes =
      Array(
        0x04, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x01, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00,
        0x03, 0x00, 0x00, 0x00
      ).map(_.toByte)
    for {
      _ <- assert.eq(bytes, ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize Array[Int]` = TestCase {
    for {
      values <- `serialize Array[Int]`
      _ <- assert.eq(values._1, ZeroFormatter.deserialize[Array[Int]](values._2)).lift
    } yield ()
  }

  case class TestElement(
    @Index(0) a: Int,
    @Index(1) b: String,
    @Index(2) c: Short
  )

  val `serialize Array[TestElement]` = TestCase {
    val value = Array(TestElement(2, "01234", 3), TestElement(4, "567890", 5))
    val bytes =
      Array(
        0x02, 0x00, 0x00, 0x00,
        0x23, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00,
        0x18, 0x00, 0x00, 0x00,
        0x1c, 0x00, 0x00, 0x00,
        0x25, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00,
        0x05, 0x00, 0x00, 0x00, 0x30, 0x31, 0x32, 0x33, 0x34,
        0x03, 0x00,

        0x24, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00,
        0x3b, 0x00, 0x00, 0x00,
        0x3f, 0x00, 0x00, 0x00,
        0x49, 0x00, 0x00, 0x00,
        0x04, 0x00, 0x00, 0x00,
        0x06, 0x00, 0x00, 0x00, 0x35, 0x36, 0x37, 0x38, 0x39, 0x30,
        0x05, 0x00
      ).map(_.toByte)
    println(ZeroFormatter.serialize(value).map(b => new scala.runtime.RichInt(b).toHexString).toList)
    for {
      _ <- assert.eq(bytes, ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  implicit val testElementEqual: Equal[TestElement] = new Equal[TestElement] {
    override def equal(a1: TestElement, a2: TestElement) =
      Equal[Int].equal(a1.a, a2.a) && Equal[String].equal(a1.b, a2.b) && Equal[Short].equal(a1.c, a2.c)
  }

  val `deserialize Array[TestElement]` = TestCase {
    for {
      values <- `serialize Array[TestElement]`
      _ <- assert.eq(values._1, ZeroFormatter.deserialize[Array[TestElement]](values._2)).lift
    } yield ()
  }
}
