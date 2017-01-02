package zeroformatter.benchmark

import dog._
import zeroformatter._
import Formatter._

object EncodingBenchmarkTest extends Base {
  val benchmark: EncodingBenchmark = new EncodingBenchmark

  import benchmark._

  val `correctly encode integers using ZeroFormatter` = TestCase {
    assert(ZeroFormatter.deserialize[List[Int]](encodeIntsZ) == ints)
  }

  //val `correctly encode case classes using ZeroFormatter` = TestCase {
  //  assert(ZeroFormatter.deserialize[Map[String, Foo]](encodeFoosZ) == foos)
  //}
}
