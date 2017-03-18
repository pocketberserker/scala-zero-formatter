package zeroformatter

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import shapeless.CaseClassMacros

abstract class Formatter[T] { self =>

  def default: T = null.asInstanceOf[T]

  def length: Option[Int]

  def serialize(encoder: Encoder, offset: Int, value: T): Int

  def deserialize(decoder: Decoder): T

  def xmap[U](f: T => U, g: U => T): Formatter[U] = new Formatter[U] {
    override def length = self.length
    override def default = f(self.default)
    override def serialize(encoder: Encoder, offset: Int, value: U) =
      self.serialize(encoder, offset, g(value))
    override def deserialize(decoder: Decoder) =
      f(self.deserialize(decoder))
  }
}

object Formatter extends FormatterInstances {

  @inline
  def apply[T](implicit F: Formatter[T]): Formatter[T] = F

  def appendLength[T](l: Option[Int])(implicit F: Formatter[T]): Option[Int] = (l, F.length) match {
    case (Some(l1), Some(l2)) => Some(l1 + l2)
    case _ => None
  }

  def serializeObjectField[T](encoder: Encoder, offset: Int, byteSize: Int, value: T, indexOffset: Int)(implicit F: Formatter[T]): Int = {
    val o = offset + byteSize
    val r = F.serialize(encoder, o, value)
    encoder.writeIntUnsafe(offset + indexOffset, o)
    byteSize + r
  }

  def serializeStructField[T](encoder: Encoder, offset: Int, byteSize: Int, value: T)(implicit F: Formatter[T]): Int =
    byteSize + F.serialize(encoder, offset + byteSize, value)

  def deserializeObjectField[T](decoder: Decoder, offset: Int, lastIndex: Int, index: Int, formatter: Formatter[T]): T =
    if(index > lastIndex) formatter.default
    else {
      val o = decoder.readInt(offset + 4 + 4 + 4 * index)
      if(o == 0) formatter.default
      else {
        decoder.offset = o
        formatter.deserialize(decoder)
      }
    }

  @inline
  def deserializeStructField[T](decoder: Decoder, formatter: Formatter[T]): T =
    formatter.deserialize(decoder)

  implicit def structOptionFormatter[A <: Struct : Formatter]: Formatter[Option[A]] =
    nullableFormatter[A]

  implicit def materialize[A]: Formatter[A] =
    macro FormatterMacros.materializeFormatter[A]
}

class FormatterMacros(val c: whitebox.Context) extends CaseClassMacros {

  import c.universe._

  def construct(tpe: Type): List[Tree] => Tree =
    args => q"${companionRef(tpe)}(..$args)"

  def markZeroFormattable(tpe: Type): Boolean = {
    val ZeroFormattable = typeOf[ZeroFormattable]
    tpe.typeSymbol.annotations.exists(ann => ann.tree.tpe =:= ZeroFormattable)
  }

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

  private[this] def length(init: Tree, fields: List[(TermName, Type)]): Tree =
    fields.foldLeft(init){ case (a, (_, t)) =>
      q"_root_.zeroformatter.Formatter.appendLength[$t]($a)"
    }

  def materializeFormatter[A: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[A]
    val ctorDtor = CtorDtor(tpe)
    val Formatter = symbolOf[Formatter[_]]
    val Encoder = symbolOf[Encoder]
    val Decoder = symbolOf[Decoder]
    val Struct = typeOf[Struct].typeSymbol

    if (markZeroFormattable(tpe)) {
      if(isProduct(tpe)) {
        val fields = fieldsOf(tpe)
        val indexes = getIndexes(tpe, fields)

        val lengthImpl =
          if(tpe.baseClasses.contains(Struct)) length(q"_root_.scala.Some(0)", fields)
          else if(indexes.nonEmpty) {
            val lastIndex = indexes.map(_._2).reduce(_ max _)
            length(q"_root_.scala.Some(${8 + (lastIndex + 1) * 4})", fields)
          }
          else q"_root_.scala.Some(12)"

        val serializeImpl =
          if(tpe.baseClasses.contains(Struct)) {
            val acc: Tree = q"0"
            fields.map { case (name, _) => name.decodedName.toString }
              .foldLeft(acc){ case (a, n) =>
                q"_root_.zeroformatter.Formatter.serializeStructField(encoder, offset, $a, value.${TermName(n)})"
              }
          }
          else if(indexes.nonEmpty) {
            val lastIndex = indexes.map(_._2).reduce(_ max _)
            // [byteSize:int(4)] + [lastIndex:int(4)] + [indexOffset...:int(4 * lastIndex)]
            val acc = q"${8 + (lastIndex + 1) * 4}"
            val r = indexes.foldLeft(acc){ case (a, (n, i)) =>
              q"_root_.zeroformatter.Formatter.serializeObjectField(encoder, offset, $a, value.${TermName(n)}, ${8 + 4 * i})"
            }
            q"""
              if(value == null) encoder.writeInt(offset, -1)
              else {
                val byteSize = $r
                encoder.writeIntUnsafe(offset + 4, $lastIndex)
                encoder.writeIntUnsafe(offset, byteSize)
                byteSize
              }
            """
          }
          else {
            q"""
              if(value == null) encoder.writeInt(offset, -1)
              else {
                val byteSize = 12
                encoder.ensureCapacity(offset, byteSize)
                encoder.writeIntUnsafe(offset, byteSize)
                encoder.writeIntUnsafe(offset + 4, 0)
                encoder.writeIntUnsafe(offset + 8, 0)
                byteSize
              }
            """
          }

        val deserializeImpl =
          if(tpe.baseClasses.contains(Struct)) {
            val r = fields.foldLeft(List[Tree]()){ case (a, (_, t)) =>
              q"_root_.zeroformatter.Formatter.deserializeStructField(decoder, _root_.zeroformatter.Formatter[$t])" :: a
            }
            q"${ctorDtor.construct(r.reverse)}"
          }
          else if(indexes.nonEmpty) {
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
          else {
            q"""
              val byteSize = decoder.readInt()
              if(byteSize == -1) null.asInstanceOf[$tpe]
              else if(byteSize < -1) throw _root_.zeroformatter.FormatException(decoder.offset - 4, "Invalid byte size(" + byteSize + ").")
              else {
                decoder.offset += 8
                ${ctorDtor.construct(List[Tree]())}
              }
            """
          }

        q"""
          new $Formatter[$tpe] {
            override val length = $lengthImpl
            override def serialize(encoder: $Encoder, offset: Int, value: $tpe) = {
              $serializeImpl
            }
            override def deserialize(decoder: $Decoder) = {
              $deserializeImpl
            }
          }
        """
      }
      else abort(s"$tpe is not case class")
    }
    else abort(s"$tpe requires to apply ZeroFormattable annotation")
  }
}
