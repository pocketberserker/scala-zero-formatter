/*

port from https://github.com/circe/circe-benchmarks/blob/3fcc6c1d8d2932ea5bccb8e322dc3aca83c952ad/src/main/scala/io/circe/benchmarks/Benchmark.scala

circe-benchmarks is licensed under the Apache License, Version 2.0 (the "License");

* change log
  * change benchmark target
  * delete printing and parsing benchmarks
  * add more data
  * List to Vector

*/

package zeroformatter.benchmark

import cats.Eval
import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

class ExampleData extends ZeroFormatterData {
  lazy val listInts: List[Int] = (0 to 1000).toList
  lazy val vecInts: Vector[Int] = (0 to 1000).toVector

  lazy val foos: Map[String, Foo] = List.tabulate(100) { i =>
    ("b" * i) -> Foo("a" * i, (i + 2.0) / (i + 1.0), i, i * 1000L, (0 to i).map(_ % 2 == 0).toVector)
  }.toMap

  lazy val bars: Map[String, Bar] = List.tabulate(100) { i =>
    ("b" * i) -> Bar(Eval.now("a" * i), Eval.now((i + 2.0) / (i + 1.0)), Eval.now(i), Eval.now(i * 1000L), Eval.now((0 to i).map(_ % 2 == 0).toVector))
  }.toMap

  val listIntsBytes: Array[Byte] = listIntsZ
  val vecIntsBytes: Array[Byte] = vecIntsZ
  val foosBytes: Array[Byte] = foosZ
  val barsBytes: Array[Byte] = barsZ
}

/**
 * Compare the performance of encoding operations.
 *
 * The following command will run the benchmarks with reasonable settings:
 *
 * > sbt "jmh:run -i 10 -wi 10 -f 2 -t 1 zeroformatter.benchmark.EncodingBenchmark"
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class EncodingBenchmark extends ExampleData with ZeroFormatterEncoding

/**
 * Compare the performance of decoding operations.
 *
 * The following command will run the benchmarks with reasonable settings:
 *
 * > sbt "jmh:run -i 10 -wi 10 -f 2 -t 1 zeroformatter.benchmark.DecodingBenchmark"
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
class DecodingBenchmark extends ExampleData with ZeroFormatterDecoding
