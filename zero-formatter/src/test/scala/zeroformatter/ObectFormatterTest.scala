package zeroformatter

import Formatter._
import dog._
import scalaz.std.anyVal._

object ObjectFormatterTest extends Base {

  case class MyClass(
    @Index(0) age: Int
    //@Index(1) firstName: String,
    //@Index(2) laftName: String
  )

  val `serialize Object` = TestCase {
    val value = MyClass(99)//, "hoge", "fuga")
    val bytes =
      Array(
        0x10, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x0c, 0x00, 0x00, 0x00,
        0x63, 0x00, 0x00, 0x00
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
