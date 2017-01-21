package zeroformatter

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import shapeless.CaseClassMacros

trait ObjectSerializer[A] {
  def serialize(encoder: Encoder, offset: Int, value: A): Int
}

trait ObjectDeserializer[A] {
  def deserialize(decoder: Decoder): A
}

object ObjectSerializer {

  implicit def materialize[A]: ObjectSerializer[A] =
    macro ObjectFormatterMacros.materializeSerializer[A]
}

object ObjectDeserializer {

  implicit def materialize[A]: ObjectDeserializer[A] =
    macro ObjectFormatterMacros.materializeDeserializer[A]
}

class ObjectFormatterMacros(val c: whitebox.Context) extends CaseClassMacros {

  import c.universe._

  def construct(tpe: Type): List[Tree] => Tree =
    args => q"${companionRef(tpe)}(..$args)"

  def getIndexes(tpe: Type, fields: List[(TermName, Type)]): List[(String, Int)] = {

    val Index = typeOf[Index]
    val construct0 = construct(Index)

    val constructorSyms = tpe
      .member(termNames.CONSTRUCTOR)
      .asMethod
      .paramLists
      .flatten
      .map { sym => sym.name.decodedName.toString -> sym }
      .toMap

    fields.map { case (name, _) =>
      val paramConstrSym = constructorSyms(name.decodedName.toString)

      paramConstrSym.annotations.collectFirst {
        case ann if ann.tree.tpe =:= Index =>
          name.decodedName.toString -> construct0(ann.tree.children.tail)
      }
    }
      .collect {
        case Some((name, annTree)) =>
          annTree match {
            case Apply(_, List(Literal(Constant(n: Int)))) if n >= 0 =>
              (name, n)
          }
      }
  }

  def materializeSerializer[A: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[A]
    val ObjectSerializer = symbolOf[ObjectSerializer[_]]
    val Encoder = symbolOf[Encoder]
    val Struct = typeOf[Struct].typeSymbol

    val serializeImpl =
      if(isProduct(tpe)) {
        val fields = fieldsOf(tpe)
        val indexes = getIndexes(tpe, fields)
        if(indexes.nonEmpty) {
          val lastIndex = indexes.map(_._2).reduce(_ max _)
          // [byteSize:int(4)] + [lastIndex:int(4)] + [indexOffset...:int(4 * lastIndex)]
          val acc = q"${4 + 4 + (lastIndex + 1) * 4}"
          val r = indexes.foldLeft(acc){ case (a, (n, i)) =>
            q"_root_.zeroformatter.Formatter.serializeObjectField(encoder, offset, $a, value.${TermName(n)}, ${4 + 4 + 4 * i})"
          }
          q"""
            val byteSize = $r
            encoder.writeIntUnsafe(offset + 4, $lastIndex)
            encoder.writeIntUnsafe(offset, byteSize)
            byteSize
          """
        }
        else if(tpe.baseClasses.contains(Struct)) {
          val acc: Tree = q"0"
          fields.map { case (name, _) => name.decodedName.toString }
            .foldLeft(acc){ case (a, n) =>
              q"_root_.zeroformatter.Formatter.serializeStructField(encoder, offset, $a, value.${TermName(n)})"
            }
        }
        else abort(s"$tpe fields require to apply Index annotation or inherit Struct")
      }
      else abort(s"$tpe is not case class")

      q"""
        new $ObjectSerializer[$tpe] {
          override def serialize(encoder: $Encoder, offset: Int, value: $tpe) = {
            $serializeImpl
          }
        }
      """
  }

  def materializeDeserializer[A: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[A]
    val ctorDtor = CtorDtor(tpe)
    val ObjectDeserializer = symbolOf[ObjectDeserializer[_]]
    val Decoder = symbolOf[Decoder]
    val Struct = typeOf[Struct].typeSymbol

    val deserializeImpl =
      if(isProduct(tpe)) {
        val fields = fieldsOf(tpe)
        val indexes = getIndexes(tpe, fields)
        if(indexes.nonEmpty) {
          val r = indexes.zip(fields.map(_._2)).foldLeft(List[Tree]()){ case (a, ((_, i), t)) =>
            q"_root_.zeroformatter.Formatter.deserializeObjectField(decoder, offset, lastIndex, $i, _root_.zeroformatter.Formatter[$t])" :: a
          }
          q"""
            val byteSize = decoder.readInt()
            if(byteSize == -1) null.asInstanceOf[$tpe]
            else if(byteSize < -1) throw _root_.zeroformatter.FormatException(decoder.offset - 4, "Invalid byte size(" + byteSize + ").")
            else {
              val offset = decoder.offset - 4
              val lastIndex = decoder.readInt()
              val result = ${ctorDtor.construct(r.reverse)}
              decoder.offset = offset + byteSize
              result
            }
          """
        }
        else if(tpe.baseClasses.contains(Struct)) {
          val r = fieldsOf(tpe).map { case (_, t) => t }
            .foldLeft(List[Tree]()){ case (a, t) =>
              q"_root_.zeroformatter.Formatter.deserializeStructField(decoder, _root_.zeroformatter.Formatter[$t])" :: a
            }
          q"${ctorDtor.construct(r.reverse)}"
        }
        else abort(s"$tpe fields require to apply Index annotation or inherit Struct")
      }
      else abort(s"$tpe is not case class")

      q"""
        new $ObjectDeserializer[$tpe] {
          override def deserialize(decoder: $Decoder) = {
            $deserializeImpl
          }
        }
      """
  }
}
