package zeroformatter.benchmark

import zeroformatter._
import dog._

object DecodingBenchmarkTest extends Base {
  val benchmark: DecodingBenchmark = new DecodingBenchmark

  import benchmark._

  val `correctly decode integers using ZeroFormatter` = TestCase {
    assert(decodeIntsZ == ints)
  }

  //val `correctly decode case classes using ZeroFormatter` = TestCase {
  //  assert(decodeFoosZ == foos)
  //}
}
