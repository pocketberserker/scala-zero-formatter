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

  @inline def unsafeEncodeZ[A](a: A)(implicit F: Formatter[A]): Array[Byte] =
    unsafe.ZeroFormatter.serialize(a)

  @inline def lz4EncodeZ[A](a: A)(implicit F: Formatter[A]): Array[Byte] =
    lz4.ZeroFormatter.serialize(a)

  val foosZ: Array[Byte] = encodeZ(foos)
  val lz4FoosZ: Array[Byte] = lz4EncodeZ(foos)
  val cachedFoos: Map[String, Accessor[Foo]] =
    foos.mapValues(f => Accessor(f, Some(foosZ)))
  val barsZ: Array[Byte] = encodeZ(bars)
  val listIntsZ: Array[Byte] = encodeZ(listInts)
  val vecIntsZ: Array[Byte] = encodeZ(vecInts)
  val lz4VecIntsZ: Array[Byte] = lz4EncodeZ(vecInts)
}

trait ZeroFormatterEncoding { self: ExampleData =>
  @Benchmark
  def encodeFoosZ: Array[Byte] = encodeZ(foos)

  @Benchmark
  def unsafeEncodeFoosZ: Array[Byte] = unsafeEncodeZ(foos)

  @Benchmark
  def encodeCachedFoosZ: Array[Byte] = encodeZ(cachedFoos)

  @Benchmark
  def lz4EncodeFoosZ: Array[Byte] = lz4EncodeZ(foos)

  @Benchmark
  def encodeBarsZ: Array[Byte] = encodeZ(bars)

  @Benchmark
  def encodeListIntsZ: Array[Byte] = encodeZ(listInts)

  @Benchmark
  def encodeVectorIntsZ: Array[Byte] = encodeZ(vecInts)

  @Benchmark
  def unsafeEncodeListIntsZ: Array[Byte] = unsafeEncodeZ(listInts)

  @Benchmark
  def unsafeEncodeVectorIntsZ: Array[Byte] = unsafeEncodeZ(vecInts)

  @Benchmark
  def lz4EncodeVectorIntsZ: Array[Byte] = lz4EncodeZ(vecInts)
}

trait ZeroFormatterDecoding { self: ExampleData =>
  @Benchmark
  def decodeFoosZ: Map[String, Foo] = ZeroFormatter.deserialize[Map[String, Foo]](foosZ)

  @Benchmark
  def unsafeDecodeFoosZ: Map[String, Foo] =
    unsafe.ZeroFormatter.deserialize[Map[String, Foo]](foosZ)

  @Benchmark
  def lz4DecodeFoosZ: Map[String, Foo] =
    lz4.ZeroFormatter.deserialize[Map[String, Foo]](lz4FoosZ)

  @Benchmark
  def decodeBarsZ: Map[String, Bar] = ZeroFormatter.deserialize[Map[String, Bar]](barsZ)

  @Benchmark
  def decodeListIntsZ: List[Int] = ZeroFormatter.deserialize[List[Int]](listIntsZ)

  @Benchmark
  def decodeVectorIntsZ: Vector[Int] = ZeroFormatter.deserialize[Vector[Int]](vecIntsZ)

  @Benchmark
  def unsafeDecodeListIntsZ: List[Int] =
    unsafe.ZeroFormatter.deserialize[List[Int]](listIntsZ)

  @Benchmark
  def unsafeDecodeVectorIntsZ: Vector[Int] =
    unsafe.ZeroFormatter.deserialize[Vector[Int]](vecIntsZ)

  @Benchmark
  def lz4DecodeVectorIntsZ: Vector[Int] =
    lz4.ZeroFormatter.deserialize[Vector[Int]](lz4VecIntsZ)
}
