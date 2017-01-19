# scala-zero-formatter

[![Build Status](https://travis-ci.org/pocketberserker/scala-zero-formatter.svg?branch=master)](https://travis-ci.org/pocketberserker/scala-zero-formatter)

implementation of [ZeroFormatter](https://github.com/neuecc/ZeroFormatter) in Scala.

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

### Union Format

| Scala | C# | Note |
| ---- | ---- | --- |
| `Union` | Union | |
