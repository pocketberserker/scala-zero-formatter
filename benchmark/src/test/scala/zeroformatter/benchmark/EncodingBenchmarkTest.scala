package zeroformatter.benchmark

import dog._
import zeroformatter._

object EncodingBenchmarkTest extends Base {
  val benchmark: EncodingBenchmark = new EncodingBenchmark

  import benchmark._

  val `correctly encode integers` = TestCase {
    assert(ZeroFormatter.deserialize[List[Int]](encodeListIntsZ) == listInts)
  }

  val `correctly encode case classes` = TestCase {
    assert(ZeroFormatter.deserialize[Map[String, Foo]](encodeFoosZ) == foos)
  }

  val `correctly encode case class with cats.Eval` = TestCase {
    val expected = bars.last._2
    val actual = ZeroFormatter.deserialize[Map[String, Bar]](encodeBarsZ).last._2
    assert.equal(expected.s.value, actual.s.value)
      .equal(expected.d.value, actual.d.value)
      .equal(expected.i.value, actual.i.value)
      .equal(expected.l.value, actual.l.value)
      .equal(expected.bs.value, actual.bs.value)
  }
}
