# scala-zero-formatter

[![Build Status](https://travis-ci.org/pocketberserker/scala-zero-formatter.svg?branch=master)](https://travis-ci.org/pocketberserker/scala-zero-formatter)

implementation of [ZeroFormatter](https://github.com/neuecc/ZeroFormatter) in Scala.

## Supported types (Stage1 only)

see also [WireFormat Specifation](https://github.com/neuecc/ZeroFormatter/tree/1.5.7#wireformat-specification).

### Primitive Format

| Scala | C# | Note |
| ---- | ---- | --- |
| `Short` | `Int16` | |
| `Int` | `Int32`| |
| `Long` | `Int64` | |
| | `UInt16` | |
| | `UInt32` | |
| | `UInt64` | |
| `Float` | `Single` | |
| `Double` | ← | |
| `Boolean` | ← | |
| | `Byte` | |
| `Byte` | `SByte` | |
| `Char` | ← | |
| `Duration` | `TimeSpan` | |
| `LocalDateTime` | `DateTime` | JVM only. |
| `OffsetDateTime` | `DateTimeOffset` | JVM only. |
| `ZonedDateTime` | `DateTimeOffset` | JVM only. `ZonedDateTime` can not deserialize Zone ID. |
| `String` | ← | |
| `Option[Short]` | `Int16?` | |
| `Option[Int]` | `Int32?`| |
| `Option[Long]` | `Int64`? | |
| | `UInt16`? | |
| | `UInt32`? | |
| | `UInt64`? | |
| `Option[Float]` | `Single?` | |
| `Option[Double]` | `Double?` | |
| `Option[Boolean]` | `Boolean?` | |
| | `Byte?` | |
| `Option[Byte]` | `SByte?` | |
| `Option[Duration]` | `TimeSpan?` | |
| `Option[LocalDateTime]` | `DateTime?` | JVM only. |
| `Option[OffsetDateTime]` | `DateTimeOffset?` | JVM only. |
| `Option[ZonedDateTime]` | `DateTimeOffset?` | JVM only. `ZonedDateTime` can not deserialize Zone ID. |
| `Option[String]` | `String?` | |

### Sequence Format

| Scala | C# | Note |
| ---- | ---- | --- |
| `Array[T]` | `Sequence<T>` | if length = -1, indicates null |
| `Option[Array[T]]` | `Sequence<T>` | if length = -1, indicates `None` |

### List Format

| Scala | C# | Note |
| ---- | ---- | --- |
| | FixedSizeList | |
| | VariableSizeList | |

### Object Format

| Scala | C# | Note |
| ---- | ---- | --- |
| Object | Object | if byteSize = -1, indicates null |
| Option[Object] | Object | if byteSize = -1, indicates `None` |
| | Struct | |
| | Struct? | |

### Union Format

| Scala | C# | Note |
| ---- | ---- | --- |
| | Union | |
