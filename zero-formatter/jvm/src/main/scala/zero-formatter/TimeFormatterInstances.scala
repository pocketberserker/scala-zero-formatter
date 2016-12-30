package zeroformatter

import java.nio.ByteBuffer
import java.time._
import Formatter._

object TimeFormatterInstances {

  implicit val localDateTimeFormatter: Formatter[LocalDateTime] = new Formatter[LocalDateTime] {
    override val length = Some(12)
    override def serialize(bytes: Array[Byte], offset: Int, value: LocalDateTime) = {
      val instant = value.toInstant(ZoneOffset.UTC)
      (intFormatter.serialize(longFormatter.serialize(bytes, offset, instant.getEpochSecond)._1, offset + 8, instant.getNano)._1, 12)
    }
    override def deserialize(buf: ByteBuffer, offset: Int) = {
      val second = longFormatter.deserialize(buf, offset).value
      val nano = intFormatter.deserialize(buf, offset + 8).value
      DeserializeResult(Instant.ofEpochSecond(second, nano).atOffset(ZoneOffset.UTC).toLocalDateTime, 12)
    }
  }

  private[this] def serializeOffsetDateTime(bytes: Array[Byte], offset: Int, value: OffsetDateTime): (Array[Byte], Int) = {
    val instant = value.toInstant
    val bs = intFormatter.serialize(longFormatter.serialize(bytes, offset, instant.getEpochSecond)._1, offset + 8, instant.getNano)._1
    val offsetMinites = (value.getOffset.getTotalSeconds / 60).toShort
    (shortFormatter.serialize(bs, offset + 12, offsetMinites)._1, 14)
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
      DeserializeResult(deserializeOffsetDateTime(buf, offset), 14)
    }
  }

  implicit val zonedDateTimeFormatter: Formatter[ZonedDateTime] = new Formatter[ZonedDateTime] {
    override val length = Some(14)
    override def serialize(bytes: Array[Byte], offset: Int, value: ZonedDateTime) =
      serializeOffsetDateTime(bytes, offset, value.toOffsetDateTime)
    override def deserialize(buf: ByteBuffer, offset: Int) = {
      DeserializeResult(deserializeOffsetDateTime(buf, offset).toZonedDateTime, 14)
    }
  }
}
