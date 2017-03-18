package zeroformatter

import dog._
import scalaz.Equal
import scalaz.std.anyVal._
import scalaz.std.string._

object LZ4Test extends Base {

  val `serialize Array[Int]` = TestCase {
    val values = (0 to 14).toArray
    assert.eq(values, lz4.ZeroFormatter.deserialize[Array[Int]](lz4.ZeroFormatter.serialize(values)))
  }

  @ZeroFormattable
  case class TestElement(
    @Index(0) a: Int,
    @Index(1) b: String,
    @Index(2) c: Short
  )

  implicit val testElementEqual: Equal[TestElement] = new Equal[TestElement] {
    override def equal(a1: TestElement, a2: TestElement) =
      Equal[Int].equal(a1.a, a2.a) && Equal[String].equal(a1.b, a2.b) && Equal[Short].equal(a1.c, a2.c)
  }

  val `serialize Array[TestElement]` = TestCase {
    val values = Array(TestElement(2, "01234", 3), TestElement(4, "567890", 5))
    assert.eq(values, lz4.ZeroFormatter.deserialize[Array[TestElement]](lz4.ZeroFormatter.serialize(values)))
  }
}
