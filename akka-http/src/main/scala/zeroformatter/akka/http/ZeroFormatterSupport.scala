package zeroformatter.akka.http

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.MediaTypes.`application/octet-stream`
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import zeroformatter._

object ZeroFormatterSupport extends ZeroFormatterSupport {
  override def serialize[A: Formatter](value: A) = unsafe.ZeroFormatter.serialize(value)
  override def deserialize[A: Formatter](bytes: Array[Byte]) = unsafe.ZeroFormatter.deserialize[A](bytes)
}

trait ZeroFormatterSupport {

  def serialize[A: Formatter](value: A): Array[Byte]

  def deserialize[A: Formatter](bytes: Array[Byte]): A

  private val zeroFormatterUnmarshaller =
    Unmarshaller.byteArrayUnmarshaller
      .forContentTypes(`application/octet-stream`)
      .map {
        case Array() => throw Unmarshaller.NoContentException
        case data => data
      }

  private val zeroFormatterMarshaller = Marshaller.byteArrayMarshaller(`application/octet-stream`)

  implicit def unmarshaller[A: Formatter]: FromEntityUnmarshaller[A] = {
    zeroFormatterUnmarshaller
      .map(bs => deserialize(bs))
  }

  implicit def marshaller[A: Formatter]: ToEntityMarshaller[A] =
    zeroFormatterMarshaller
      .compose(v => serialize(v))
}
