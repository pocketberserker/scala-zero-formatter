package zeroformatter.benchmark

import zeroformatter._
import dog._

object DecodingBenchmarkTest extends Base {
  val benchmark: DecodingBenchmark = new DecodingBenchmark

  import benchmark._

  val `correctly decode integers` = TestCase {
    assert(decodeIntsZ == ints)
  }

  val `correctly decode case classes` = TestCase {
    assert(decodeFoosZ == foos)
  }

  val `correctly decode case class with cats.Eval` = TestCase {
    val expected = bars.last._2
    val actual = decodeBarsZ.last._2
    assert.equal(expected.s.value, actual.s.value)
      .equal(expected.d.value, actual.d.value)
      .equal(expected.i.value, actual.i.value)
      .equal(expected.l.value, actual.l.value)
      .equal(expected.bs.value, actual.bs.value)
  }
}
