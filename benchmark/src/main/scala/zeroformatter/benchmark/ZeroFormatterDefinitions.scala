package zeroformatter.benchmark

import shapeless._
import zeroformatter._
import org.openjdk.jmh.annotations._

trait ZeroFormatterFooInstances {
  implicit val zeroFormatterFoo: Formatter[Foo] = cachedImplicit
}

trait ZeroFormatterBarInstances {
  import zeroformatter.cats._
  implicit val zeroFormatterFoo: Formatter[Bar] = cachedImplicit
}

trait ZeroFormatterData { self: ExampleData =>
  @inline def encodeZ[A](a: A)(implicit F: Formatter[A]): Array[Byte] =
    ZeroFormatter.serialize(a)

  val foosZ: Array[Byte] = encodeZ(foos)
  val barsZ: Array[Byte] = encodeZ(bars)
  val intsZ: Array[Byte] = encodeZ(ints)
}

trait ZeroFormatterEncoding { self: ExampleData =>
  @Benchmark
  def encodeFoosZ: Array[Byte] = encodeZ(foos)

  @Benchmark
  def encodeBarsZ: Array[Byte] = encodeZ(bars)

  @Benchmark
  def encodeIntsZ: Array[Byte] = encodeZ(ints)
}

trait ZeroFormatterDecoding { self: ExampleData =>
  @Benchmark
  def decodeFoosZ: Map[String, Foo] = ZeroFormatter.deserialize[Map[String, Foo]](foosZ)

  @Benchmark
  def decodeBarsZ: Map[String, Bar] = ZeroFormatter.deserialize[Map[String, Bar]](barsZ)

  @Benchmark
  def decodeIntsZ: List[Int] = ZeroFormatter.deserialize[List[Int]](intsZ)
}
