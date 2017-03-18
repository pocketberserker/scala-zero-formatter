# scala-zero-formatter

[![Build Status](https://travis-ci.org/pocketberserker/scala-zero-formatter.svg?branch=master)](https://travis-ci.org/pocketberserker/scala-zero-formatter)
[![Scala.js](https://www.scala-js.org/assets/badges/scalajs-0.6.14.svg)](https://www.scala-js.org)

Implementation of [ZeroFormatter](https://github.com/neuecc/ZeroFormatter) in Scala.

## Latest stable version

```
libraryDependencies += "com.github.pocketberserker" %% "zero-formatter" % "0.6.0"
```

```
// JVM only
libraryDependencies += "com.github.pocketberserker" %% "zero-formatter-unsafe" % "0.6.0"
```

```
libraryDependencies += "com.github.pocketberserker" %% "zero-formatter-scalaz" % "0.6.0"
```

```
libraryDependencies += "com.github.pocketberserker" %% "zero-formatter-cats-core" % "0.6.0"
```

```
// JVM only
libraryDependencies += "com.github.pocketberserker" %% "akka-http-zero-formatter" % "0.6.0"
```

```
// JVM only
libraryDependencies += "com.github.pocketberserker" %% "zero-formatter-lz4" % "0.6.0"
```

## Usage

Define case class and fields mark as `@Index`, call `ZeroFormatter.serialize[T]/deserialize[T}`

```scala
import zeroformatter._

@ZeroFormattable
case class MyClass(
  @Index(0) age: Int,
  @Index(1) firstName: String,
  @Index(2) lastName: String,
  @Index(3) list: Vector[Int]
)

val mc = MyClass(99, "hoge", "fuga", Vector(1, 10, 100))

val bytes = ZeroFormatter.serialize(mc)
val mc2 = ZeroFormatter.deserialize[MyClass](bytes)
```

## lazy-evaluation

If you use `cats.Eval`, case class deserialization is lazy-evaluation.

```scala
import cats.Eval
import zeroformatter._
import zeroformatter.cats._

@ZeroFormattable
case class LazyClass(
  @Index(0) age: Eval[Int],
  @Index(1) firstName: Eval[String],
  @Index(2) lastName: Eval[String],
  @Index(3) list: Eval[Vector[Int]]
)

val lc = ZeroFormatter.deserialize[LazyClass](bytes)
```

### Caution

lazy-evaluation deserialization is supported only `Object` and `LazyList`.

## Supported types

see also [WireFormat Specification](https://github.com/neuecc/ZeroFormatter/tree/1.5.7#wireformat-specification).

### Primitive Format

| Scala | C# | Note |
| ---- | ---- | --- |
| `Short` | `Int16` | |
| `Int` | `Int32`| |
| `Long` | `Int64` | |
| `spire.math.UShort` | `UInt16` | |
| `spire.math.UInt` | `UInt32` | |
| `spire.math.ULong` | `UInt64` | |
| `Float` | `Single` | |
| `Double` | ← | |
| `Boolean` | ← | |
| `spire.math.UByte` | `Byte` | |
| `Byte` | `SByte` | |
| `Char` | ← | |
| `Duration` | `TimeSpan` | |
| `LocalDateTime` | `DateTime` | JVM only. |
| `OffsetDateTime` | `DateTimeOffset` | JVM only. |
| `String` | ← | Scala and C# string is always nullable currently. |
| `Option[Short]` | `Int16?` | |
| `Option[Int]` | `Int32?`| |
| `Option[Long]` | `Int64?` | |
| `Option[spire.math.UShort]` | `UInt16?` | |
| `Option[spire.math.UInt]` | `UInt32?` | |
| `Option[spire.math.ULong]` | `UInt64?` | |
| `Option[Float]` | `Single?` | |
| `Option[Double]` | `Double?` | |
| `Option[Boolean]` | `Boolean?` | |
| `Option[spire.math.UByte]` | `Byte?` | |
| `Option[Byte]` | `SByte?` | |
| `Option[Duration]` | `TimeSpan?` | |
| `Option[LocalDateTime]` | `DateTime?` | JVM only. |
| `Option[OffsetDateTime]` | `DateTimeOffset?` | JVM only. |
| `Option[String]` | | |

### Sequence Format

| Scala | C# | Note |
| ---- | ---- | --- |
| `Array[T]` | `Sequence<T>` | if length = -1, indicates null |
| `Option[Array[T]]` | `Sequence<T>` | if length = -1, indicates `None` |

### List Format

| Scala | C# | Note |
| ---- | ---- | --- |
| `LazyList` | FixedSizeList | Stage2 support only `zeroformatter-cats-core` module |
| `LazyList` | VariableSizeList | Stage2 support only `zeroformatter-cats-core` module |

### Object Format

| Scala | C# | Note |
| ---- | ---- | --- |
| Object | Object | if byteSize = -1, indicates null |
| Option[Object] | Object | if byteSize = -1, indicates `None` |
| `Struct` | Struct | |
| `Option[Struct]` | Struct? | |
| `Option[(A1, A2)]` | Tuple<A1, A2> | |

### Union Format

| Scala | C# | Note |
| ---- | ---- | --- |
| `Union` | Union | |

