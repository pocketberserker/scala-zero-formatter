package zeroformatter.benchmark

import zeroformatter._
import zeroformatter.Formatter._
import org.openjdk.jmh.annotations._

trait ZeroFormatterFooInstances {
  implicit val zeroFormatterFoo: Formatter[Foo] = Formatter[Foo]
}

trait ZeroFormatterData { self: ExampleData =>
  @inline def encodeZ[A](a: A)(implicit F: Formatter[A]): Array[Byte] =
    ZeroFormatter.serialize(a)

  //val foosZ: Array[Byte] = encodeZ(foos)
  val intsZ: Array[Byte] = encodeZ(ints)
}

trait ZeroFormatterEncoding { self: ExampleData =>
  //@Benchmark
  //def encodeFoosZ: Array[Byte] = encodeZ(foos)

  @Benchmark
  def encodeIntsZ: Array[Byte] = encodeZ(ints)
}

trait ZeroFormatterDecoding { self: ExampleData =>
  //@Benchmark
  //def decodeFoosZ: Map[String, Foo] = ZeroFormatter.deserialize[Map[String, Foo]](foosZ)

  @Benchmark
  def decodeIntsZ: List[Int] = ZeroFormatter.deserialize[List[Int]](intsZ)
}
