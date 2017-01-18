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
  val listIntsZ: Array[Byte] = encodeZ(listInts)
  val vecIntsZ: Array[Byte] = encodeZ(vecInts)
}

trait ZeroFormatterEncoding { self: ExampleData =>
  @Benchmark
  def encodeFoosZ: Array[Byte] = encodeZ(foos)

  @Benchmark
  def encodeBarsZ: Array[Byte] = encodeZ(bars)

  @Benchmark
  def encodeListIntsZ: Array[Byte] = encodeZ(listInts)

  @Benchmark
  def encodeVectorIntsZ: Array[Byte] = encodeZ(vecInts)
}

trait ZeroFormatterDecoding { self: ExampleData =>
  @Benchmark
  def decodeFoosZ: Map[String, Foo] = ZeroFormatter.deserialize[Map[String, Foo]](foosZ)

  @Benchmark
  def decodeBarsZ: Map[String, Bar] = ZeroFormatter.deserialize[Map[String, Bar]](barsZ)

  @Benchmark
  def decodeListIntsZ: List[Int] = ZeroFormatter.deserialize[List[Int]](listIntsZ)

  @Benchmark
  def decodeVectorIntsZ: Vector[Int] = ZeroFormatter.deserialize[Vector[Int]](vecIntsZ)
}
