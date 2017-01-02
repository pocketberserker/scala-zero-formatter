package zeroformatter.benchmark

import dog._
import zeroformatter._
import Formatter._

object EncodingBenchmarkSpec extends Base {
  val benchmark: EncodingBenchmark = new EncodingBenchmark

  import benchmark._

  val `correctly encode integers using ZeroFormatter` = TestCase {
    assert(ZeroFormatter.deserialize[List[Int]](encodeIntsZ) == ints)
  }

  //val `correctly encode case classes using ZeroFormatter` = TestCase {
  //  assert(decodeFoos(encodeFoosZ) == Some(foos))
  //}
}
