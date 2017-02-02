package object zeroformatter extends FormatterInstances {

  implicit def caseClassFormatter[A](implicit
    serializer: ObjectSerializer[A],
    deserializer: ObjectDeserializer[A]
  ): Formatter[A] = new Formatter[A] {

    override val length = None

    override def serialize(encoder: Encoder, offset: Int, value: A) =
      serializer.serialize(encoder, offset, value)

    override def deserialize(decoder: Decoder) =
      deserializer.deserialize(decoder)
  }

  implicit def structOptionFormatter[A <: Struct : Formatter]: Formatter[Option[A]] =
    nullableFormatter[A]
}
