package zeroformatter

import Formatter._
import dog._
import scalaz.std.anyVal._

sealed abstract class TestEnum(override val label: Int) extends Enum[Int] with Product with Serializable
case class Enum0() extends TestEnum(1)
case class Enum1() extends TestEnum(2)

object EnumFormatterTest extends Base {

  val `serialize Enum` = TestCase {
    val value: TestEnum = Enum0()
    val bytes = Array(0x01, 0x00, 0x00, 0x00).map(_.toByte)
    for {
      _ <- assert.eq(bytes, ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize Enum` = TestCase {
    for {
      values <- `serialize Enum`
      _ <- assert.equal(values._1, ZeroFormatter.deserialize[TestEnum](values._2)).lift
    } yield ()
  }
}
