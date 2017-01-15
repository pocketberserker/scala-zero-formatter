package zeroformatter

import java.nio.ByteBuffer
import java.time._
import Formatter._

object TimeFormatterInstances {

  implicit val localDateTimeFormatter: Formatter[LocalDateTime] = new Formatter[LocalDateTime] {
    override val length = Some(12)
    override def serialize(bytes: Array[Byte], offset: Int, value: LocalDateTime) = {
      val instant = value.toInstant(ZoneOffset.UTC)
      LazyResult(
        intFormatter.serialize(longFormatter.serialize(bytes, offset, instant.getEpochSecond).value, offset + 8, instant.getNano).value,
        12
      )
    }
    override def deserialize(buf: ByteBuffer, offset: Int) = {
      val second = longFormatter.deserialize(buf, offset).value
      val nano = intFormatter.deserialize(buf, offset + 8).value
      LazyResult(Instant.ofEpochSecond(second, nano).atOffset(ZoneOffset.UTC).toLocalDateTime, 12)
    }
  }

  private[this] def serializeOffsetDateTime(bytes: Array[Byte], offset: Int, value: OffsetDateTime) = {
    val instant = value.toInstant
    val bs = intFormatter.serialize(longFormatter.serialize(bytes, offset, instant.getEpochSecond).value, offset + 8, instant.getNano).value
    val offsetMinites = (value.getOffset.getTotalSeconds / 60).toShort
    LazyResult(shortFormatter.serialize(bs, offset + 12, offsetMinites).value, 14)
  }

  private[this] def deserializeOffsetDateTime(buf: ByteBuffer, offset: Int): OffsetDateTime = {
    val second = longFormatter.deserialize(buf, offset).value
    val nano = intFormatter.deserialize(buf, offset + 8).value
    val offsetMinites = shortFormatter.deserialize(buf, offset + 12).value.toInt
    Instant.ofEpochSecond(second, nano).atOffset(ZoneOffset.ofTotalSeconds(offsetMinites * 60))
  }

  implicit val offsetDateTimeFormatter: Formatter[OffsetDateTime] = new Formatter[OffsetDateTime] {
    override val length = Some(14)
    override def serialize(bytes: Array[Byte], offset: Int, value: OffsetDateTime) =
      serializeOffsetDateTime(bytes, offset, value)
    override def deserialize(buf: ByteBuffer, offset: Int) = {
      LazyResult(deserializeOffsetDateTime(buf, offset), 14)
    }
  }

  implicit val localDateTimeOptionFormatter: Formatter[Option[LocalDateTime]] = nullableFormatter[LocalDateTime]
  implicit val offsetDateTimeOptionFormatter: Formatter[Option[OffsetDateTime]] = nullableFormatter[OffsetDateTime]
}
