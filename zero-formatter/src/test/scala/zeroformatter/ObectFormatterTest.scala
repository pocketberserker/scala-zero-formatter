package zeroformatter

import Formatter._
import dog._
import scalaz.std.anyVal._

object ObjectFormatterTest extends Base {

  case class MyClass(
    @Index(0) age: Int,
    @Index(1) firstName: String,
    @Index(2) laftName: String
  )

  val `serialize Object` = TestCase {
    val value = MyClass(99, "hoge", "fuga")
    val bytes =
      Array(
        0x28, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00,
        0x14, 0x00, 0x00, 0x00,
        0x18, 0x00, 0x00, 0x00,
        0x20, 0x00, 0x00, 0x00,
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

  val `deserialize Object` = TestCase {
    for {
      values <- `serialize Object`
      _ <- assert.equal(values._1, ZeroFormatter.deserialize[MyClass](values._2)).lift
    } yield ()
  }
}
