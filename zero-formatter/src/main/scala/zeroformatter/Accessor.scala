package zeroformatter

import scala.language.dynamics
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

final case class Accessor[A: Formatter](
  value: A,
  cache: Option[Array[Byte]] = None,
  offset: Int = 0,
  byteSize: Int = 0,
  updated: Boolean = false
) extends Dynamic {

  def selectDynamic(field: String): Any =
    macro AccessorMacros.selectDynamic[A]

  def updateDynamic(field: String)(value: Any): Any =
    macro AccessorMacros.updateDynamic[A]
}

object Accessor {

  implicit def accessorFormatter[A](implicit F: Formatter[A]): Formatter[Accessor[A]] = new Formatter[Accessor[A]] {

    override val length = F.length

    override def serialize(encoder: Encoder, offset: Int, value: Accessor[A]) =
      value.cache match {
        case Some(_) if value.updated =>
          F.serialize(encoder, offset, value.value)
        case Some(c) =>
          encoder.writeByteArray(offset, c, value.offset, value.byteSize)
        case None =>
          F.serialize(encoder, offset, value.value)
    }

    override def deserialize(decoder: Decoder) = {
      val o = decoder.offset
      Accessor(
        F.deserialize(decoder),
        Some(decoder.buffer),
        o,
        decoder.offset - o
      )
    }
  }
}

class AccessorMacros(override val c: whitebox.Context) extends ObjectFormatterMacros(c) {

  import c.universe._

  def nameLiteral(field: Tree): String =
    field match {
      case Literal(Constant(value: String)) => value
      case _ => abort(s"Expression $field does not evaluate to String literal")
    }

  def selectDynamic[A: WeakTypeTag](field: Tree): Tree = {
    val tpe = weakTypeOf[A]
    val name = nameLiteral(field)

    if(isProduct(tpe)) {
      if(fieldsOf(tpe).exists { case (n, t) => n.decodedName.toString == name }) {
        q"${c.prefix}.value.${TermName(name)}"
      }
      else abort(s"$tpe#$name not found")
    }
    else abort(s"$tpe is not case class")
  }

  def updateDynamic[A: WeakTypeTag](field: Tree)(value: Tree): Tree = {
    val tpe = weakTypeOf[A]
    val ctorDtor = CtorDtor(weakTypeOf[Accessor[A]])
    val Struct = typeOf[Struct].typeSymbol
    val self = c.prefix
    val name = nameLiteral(field)
    val fields = fieldsOf(tpe)

    if(isProduct(tpe)) {
      val indexes = getIndexes(tpe, fields)
      if(indexes.nonEmpty) {
        indexes.collectFirst {
          case (n, _) if n == name => n
        }
          .getOrElse(abort(s"$tpe#$name not found"))
      }
      else if(tpe.baseClasses.contains(Struct)){
        fields.collectFirst { case (n, _) if  n.decodedName.toString == name =>
          n
        }
          .getOrElse(abort(s"$tpe#$name not found"))
      }
      else abort(s"$tpe is not case class")

      ctorDtor.construct(List(
        q"$self.value.copy(${TermName(name)} = $value)",
        q"updated = true"
      ))
    }
    else abort(s"$tpe is not case class")
  }
}
