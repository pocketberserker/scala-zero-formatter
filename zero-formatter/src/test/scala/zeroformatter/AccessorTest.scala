package zeroformatter

import dog._
import scalaz.std.anyVal._

object AccessorTest extends Base {

  @ZeroFormattable
  case class MyClass(
    @Index(0) age: Int,
    @Index(1) firstName: String,
    @Index(2) lastName: String
  )

  val `get field` = TestCase {
    val value = Accessor(MyClass(99, "hoge", "fuga"))
    assert.eq(value.age, 99)
      .equal(value.firstName, "hoge")
      .equal(value.lastName, "fuga")
  }

  val `update field` = TestCase {
    val value = Accessor(MyClass(99, "hoge", "fuga"))
    val actual = (value.age = value.age + 1)
    assert.eq(actual.age, 100)
  }

  val `serialize Accessor` = TestCase {
    val value = Accessor(MyClass(99, "hoge", "fuga"))
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

  val `deserialize Accessor` = TestCase {
    for {
      values <- `serialize Accessor`
      _ <- assert.equal(
        values._1.copy(cache = Some(values._2)).copy(byteSize = 0x28),
        ZeroFormatter.deserialize[Accessor[MyClass]](values._2)
      ).lift
    } yield ()
  }

  val `update and serialize Accessor` = TestCase {
    val bytes =
      Array(
        0x28, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00,
        0x14, 0x00, 0x00, 0x00,
        0x18, 0x00, 0x00, 0x00,
        0x20, 0x00, 0x00, 0x00,
        0x64, 0x00, 0x00, 0x00,
        0x04, 0x00, 0x00, 0x00,
        0x68, 0x6f, 0x67, 0x65,
        0x04, 0x00, 0x00, 0x00,
        0x66, 0x75, 0x67, 0x61
      ).map(_.toByte)
    for {
      values <- `serialize Accessor`
      d = ZeroFormatter.deserialize[Accessor[MyClass]](values._2)
      target = (d.age = d.age + 1)
      _ <- assert.eq(bytes, ZeroFormatter.serialize(target)).lift
    } yield ()
  }
}
