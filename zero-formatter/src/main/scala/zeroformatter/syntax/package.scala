package zeroformatter

package object syntax {

  implicit final class FormatterSyntax[A: Formatter](val value: A) {

    def serialize: Array[Byte] = ZeroFormatter.serialize(value)
  }
}
