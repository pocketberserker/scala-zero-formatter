package zeroformatter

import java.time._

abstract class FormatterInstances extends FormatterInstances0 {

  implicit val localDateTimeFormatter: Formatter[LocalDateTime] = new Formatter[LocalDateTime] {
    override val length = Some(12)
    override def serialize(bytes: Array[Byte], offset: Int, value: LocalDateTime) = {
      val instant = value.toInstant(ZoneOffset.UTC)
      LazyResult(
        intFormatter.serialize(longFormatter.serialize(bytes, offset, instant.getEpochSecond).value, offset + 8, instant.getNano).value,
        12
      )
    }
    override def deserialize(decoder: Decoder) = {
      val second = longFormatter.deserialize(decoder)
      val nano = intFormatter.deserialize(decoder)
      Instant.ofEpochSecond(second, nano).atOffset(ZoneOffset.UTC).toLocalDateTime
    }
  }

  private[this] def serializeOffsetDateTime(bytes: Array[Byte], offset: Int, value: OffsetDateTime) = {
    val instant = value.toInstant
    val bs = intFormatter.serialize(longFormatter.serialize(bytes, offset, instant.getEpochSecond).value, offset + 8, instant.getNano).value
    val offsetMinites = (value.getOffset.getTotalSeconds / 60).toShort
    LazyResult(shortFormatter.serialize(bs, offset + 12, offsetMinites).value, 14)
  }

  private[this] def deserializeOffsetDateTime(decoder: Decoder): OffsetDateTime = {
    val second = longFormatter.deserialize(decoder)
    val nano = intFormatter.deserialize(decoder)
    val offsetMinites = shortFormatter.deserialize(decoder).toInt
    Instant.ofEpochSecond(second, nano).atOffset(ZoneOffset.ofTotalSeconds(offsetMinites * 60))
  }

  implicit val offsetDateTimeFormatter: Formatter[OffsetDateTime] = new Formatter[OffsetDateTime] {
    override val length = Some(14)
    override def serialize(bytes: Array[Byte], offset: Int, value: OffsetDateTime) =
      serializeOffsetDateTime(bytes, offset, value)
    override def deserialize(decoder: Decoder) = {
      deserializeOffsetDateTime(decoder)
    }
  }

  implicit val localDateTimeOptionFormatter: Formatter[Option[LocalDateTime]] = nullableFormatter[LocalDateTime]
  implicit val offsetDateTimeOptionFormatter: Formatter[Option[OffsetDateTime]] = nullableFormatter[OffsetDateTime]
}
