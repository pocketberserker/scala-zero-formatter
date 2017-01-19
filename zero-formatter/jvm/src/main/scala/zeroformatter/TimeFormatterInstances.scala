package zeroformatter

import java.time._

abstract class FormatterInstances extends FormatterInstances0 {

  implicit val localDateTimeFormatter: Formatter[LocalDateTime] = new Formatter[LocalDateTime] {
    override val length = Some(12)
    override def serialize(encoder: Encoder, offset: Int, value: LocalDateTime) = {
      val instant = value.toInstant(ZoneOffset.UTC)
      encoder.ensureCapacity(offset, 12)
      val r1 = encoder.writeLongUnsafe(offset, instant.getEpochSecond)
      val r2 = encoder.writeIntUnsafe(offset + 8, instant.getNano)
      r1 + r2
    }
    override def deserialize(decoder: Decoder) = {
      val second = longFormatter.deserialize(decoder)
      val nano = intFormatter.deserialize(decoder)
      Instant.ofEpochSecond(second, nano).atOffset(ZoneOffset.UTC).toLocalDateTime
    }
  }

  private[this] def serializeOffsetDateTime(encoder: Encoder, offset: Int, value: OffsetDateTime) = {
    val instant = value.toInstant
    val offsetMinites = (value.getOffset.getTotalSeconds / 60).toShort
    encoder.ensureCapacity(offset, 14)
    val r1 = encoder.writeLongUnsafe(offset, instant.getEpochSecond)
    val r2 = encoder.writeIntUnsafe(offset + 8, instant.getNano)
    val r3 = encoder.writeShortUnsafe(offset + 12, offsetMinites)
    r1 + r2 + r3
  }

  private[this] def deserializeOffsetDateTime(decoder: Decoder): OffsetDateTime = {
    val second = decoder.getLong()
    val nano = decoder.getInt()
    val offsetMinites = decoder.getShort().toInt
    Instant.ofEpochSecond(second, nano).atOffset(ZoneOffset.ofTotalSeconds(offsetMinites * 60))
  }

  implicit val offsetDateTimeFormatter: Formatter[OffsetDateTime] = new Formatter[OffsetDateTime] {
    override val length = Some(14)
    override def serialize(encoder: Encoder, offset: Int, value: OffsetDateTime) =
      serializeOffsetDateTime(encoder, offset, value)
    override def deserialize(decoder: Decoder) = {
      deserializeOffsetDateTime(decoder)
    }
  }

  implicit val localDateTimeOptionFormatter: Formatter[Option[LocalDateTime]] = nullableFormatter[LocalDateTime]
  implicit val offsetDateTimeOptionFormatter: Formatter[Option[OffsetDateTime]] = nullableFormatter[OffsetDateTime]
}
