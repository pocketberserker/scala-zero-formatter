package zeroformatter.akka.http

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.MediaTypes.`application/octet-stream`
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import zeroformatter._

object ZeroFormatterSupport extends ZeroFormatterSupport

trait ZeroFormatterSupport {

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
      .map(bs => unsafe.ZeroFormatter.deserialize(bs))
  }

  implicit def marshaller[A: Formatter]: ToEntityMarshaller[A] =
    zeroFormatterMarshaller
      .compose(v => unsafe.ZeroFormatter.serialize(v))
}