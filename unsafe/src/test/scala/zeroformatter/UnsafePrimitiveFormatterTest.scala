package zeroformatter

import dog._
import dog.props._
import scalaprops._
import scalaz.std.anyVal._
import scalaz.std.string._

object UnsafePrimitiveFormatterTest extends Base {

  implicit val boolCase = Gen.elements((true, Array(1).map(_.toByte)), (false, Array(0).map(_.toByte)))

  val `serialize Boolean` = Prop.property((v: (Boolean, Array[Byte])) =>
    assert.eq(v._2, unsafe.ZeroFormatter.serialize(v._1))
  )

  val `deserialize Boolean` = Prop.property((v: (Boolean, Array[Byte])) =>
    assert.eq(v._1, unsafe.ZeroFormatter.deserialize[Boolean](v._2))
  )

  val `serialize Byte` = TestCase {
    val value = 123.toByte
    val bytes = Array(0x7b.toByte)
    for {
      _ <- assert.eq(bytes, unsafe.ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize Byte` = TestCase {
    for {
      values <- `serialize Byte`
      _ <- assert.eq(values._1, unsafe.ZeroFormatter.deserialize[Byte](values._2)).lift
    } yield ()
  }

  val `serialize Short` = TestCase {
    val value = 123.toShort
    val bytes = Array(0x7b, 0x00).map(_.toByte)
    for {
      _ <- assert.eq(bytes, unsafe.ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize Short` = TestCase {
    for {
      values <- `serialize Short`
      _ <- assert.eq(values._1, unsafe.ZeroFormatter.deserialize[Short](values._2)).lift
    } yield ()
  }

  val `serialize Int` = TestCase {
    val value = 123
    val bytes = Array(0x7b, 0x00, 0x00, 0x00).map(_.toByte)
    for {
      _ <- assert.eq(bytes, unsafe.ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize Int` = TestCase {
    for {
      values <- `serialize Int`
      _ <- assert.eq(values._1, unsafe.ZeroFormatter.deserialize[Int](values._2)).lift
    } yield ()
  }

  val `serialize Long` = TestCase {
    val value = 123.toLong
    val bytes = Array(0x7b, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00).map(_.toByte)
    for {
      _ <- assert.eq(bytes, unsafe.ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize Long` = TestCase {
    for {
      values <- `serialize Long`
      _ <- assert.eq(values._1, unsafe.ZeroFormatter.deserialize[Long](values._2)).lift
    } yield ()
  }

  val `serialize Float` = TestCase {
    val value = 123.0.toFloat
    val bytes = Array(0x00, 0x00, 0xf6, 0x42).map(_.toByte)
    for {
      _ <- assert.eq(bytes, unsafe.ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize Float` = TestCase {
    for {
      values <- `serialize Float`
      _ <- assert.eq(values._1, unsafe.ZeroFormatter.deserialize[Float](values._2)).lift
    } yield ()
  }

  val `serialize Double` = TestCase {
    val value = 123.0
    val bytes = Array(0x00, 0x00, 0x00, 0x00, 0x00, 0xc0, 0x5e, 0x40).map(_.toByte)
    for {
      _ <- assert.eq(bytes, unsafe.ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize Double` = TestCase {
    for {
      values <- `serialize Double`
      _ <- assert.eq(values._1, unsafe.ZeroFormatter.deserialize[Double](values._2)).lift
    } yield ()
  }

  val `serialize Char` = TestCase {
    val value = 'あ'
    val bytes = Array(0x42, 0x30).map(_.toByte)
    for {
      _ <- assert.eq(bytes, unsafe.ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  val `deserialize Char` = TestCase {
    for {
      values <- `serialize Char`
      _ <- assert.eq(values._1, unsafe.ZeroFormatter.deserialize[Char](values._2)).lift
    } yield ()
  }

  val `serialize and deserialize String` = TestCase {
    val value = "あいうえお"
    val r = unsafe.ZeroFormatter.serialize(value)
    assert.eq(value, unsafe.ZeroFormatter.deserialize[String](r))
  }
}
