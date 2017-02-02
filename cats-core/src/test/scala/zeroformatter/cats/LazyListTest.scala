package zeroformatter
package cats

import dog._
import scalaz.Equal
import scalaz.std.anyVal._
import scalaz.std.vector._
import scalaz.std.string._
import _root_.cats.Eval

object LazyListTest extends Base {

  val `serialize and deserialize FixedSizeList` = TestCase {
    for {
      values <- SequenceFormatterTest.`serialize Array[Int]`
      value = LazyList(values._1.toVector.map(v => Eval.now(v)))
      bytes = values._2
      actualBytes = ZeroFormatter.serialize(value)
      _ <- assert.eq(bytes, actualBytes).lift
      _ <- assert.eq(
        value.toVector.map(_.value),
        ZeroFormatter.deserialize[LazyList[Eval, Int]](actualBytes)
          .toVector.map(_.value)
      ).lift
    } yield (value, values._2)
  }

  @ZeroFormattable
  case class TestElement(
    @Index(0) a: Int,
    @Index(1) b: String,
    @Index(2) c: Short
  )

  val `serialize VariableSizeList` = TestCase {
      val value = LazyList(Vector(TestElement(2, "01234", 3), TestElement(4, "567890", 5)).map(v => Eval.now(v)))
    val bytes =
      Array(
        0x57, 0x00, 0x00, 0x00,

        0x02, 0x00, 0x00, 0x00,

        0x10, 0x00, 0x00, 0x00,
        0x33, 0x00, 0x00, 0x00,

        0x23, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00,
        0x24, 0x00, 0x00, 0x00,
        0x28, 0x00, 0x00, 0x00,
        0x31, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00,
        0x05, 0x00, 0x00, 0x00, 0x30, 0x31, 0x32, 0x33, 0x34,
        0x03, 0x00,

        0x24, 0x00, 0x00, 0x00,
        0x02, 0x00, 0x00, 0x00,
        0x47, 0x00, 0x00, 0x00,
        0x4b, 0x00, 0x00, 0x00,
        0x55, 0x00, 0x00, 0x00,
        0x04, 0x00, 0x00, 0x00,
        0x06, 0x00, 0x00, 0x00, 0x35, 0x36, 0x37, 0x38, 0x39, 0x30,
        0x05, 0x00
      ).map(_.toByte)
    for {
      _ <- assert.eq(bytes, ZeroFormatter.serialize(value)).lift
    } yield (value, bytes)
  }

  implicit val testElementEqual: Equal[TestElement] = new Equal[TestElement] {
    override def equal(a1: TestElement, a2: TestElement) =
      Equal[Int].equal(a1.a, a2.a) && Equal[String].equal(a1.b, a2.b) && Equal[Short].equal(a1.c, a2.c)
  }

  val `deserialize VariableSizeList` = TestCase {
    for {
      values <- `serialize VariableSizeList`
      expected = values._1
      actual = ZeroFormatter.deserialize[LazyList[Eval, TestElement]](values._2)
      _ <- assert.eq(expected.toVector.map(_.value), actual.toVector.map(_.value)).lift
    } yield ()
  }
}
