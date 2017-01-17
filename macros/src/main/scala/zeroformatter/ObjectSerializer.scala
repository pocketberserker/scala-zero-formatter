package zeroformatter

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import shapeless.CaseClassMacros

trait ObjectSerializer[A] extends Serializable {
  def serialize(acc: ObjectSerializerResult, value: A): ObjectSerializerResult
}

object ObjectSerializer {

  implicit def materialize[A]: ObjectSerializer[A] =
    macro ObjectSerializerMacros.materializeSerializer[A]
}

class ObjectSerializerMacros(val c: whitebox.Context) extends CaseClassMacros {

  import c.universe._

  def materializeSerializer[A: c.WeakTypeTag]: Tree = {
    val tpe = weakTypeOf[A]
    val Index = typeOf[Index]
    val ObjectSerializer = symbolOf[ObjectSerializer[_]]
    val ObjectSerializerResult = symbolOf[ObjectSerializerResult]

    val construct0: List[Tree] => Tree =
      args => q"${companionRef(Index)}(..$args)"

    val constructorSyms = tpe
      .member(termNames.CONSTRUCTOR)
      .asMethod
      .paramLists
      .flatten
      .map { sym => sym.name.decodedName.toString -> sym }
      .toMap

    val serializeImpl =
      if(isProduct(tpe)) {
        val fields = fieldsOf(tpe)
        val indexes =
          fields.map { case (name, _) =>
            val paramConstrSym = constructorSyms(name.decodedName.toString)

            paramConstrSym.annotations.collectFirst {
              case ann if ann.tree.tpe =:= Index =>
                name.decodedName.toString -> construct0(ann.tree.children.tail)
            }
          }
            .collect {
              case Some((name, annTree)) => (name, q"$annTree.value")
           }
        val acc = c.Expr[ObjectSerializerResult](Ident(TermName("acc"))).tree
        if(indexes.nonEmpty) indexes.foldLeft(acc){ case (a, (n, i)) =>
        q"_root_.zeroformatter.SerializeHelper.serializeObjectField($a, value.${TermName(n)}, $i)"
        }
        else
          fields.map { case (name, _) => name.decodedName.toString }
            .foldLeft(acc){ case (a, n) =>
              q"_root_.zeroformatter.SerializeHelper.serializeStructField($a, value.${TermName(n)})"
            }
      }
      else abort(s"$tpe is not case class")

      q"""
        new $ObjectSerializer[$tpe] {
          def serialize(acc: $ObjectSerializerResult, value: $tpe) = {
            $serializeImpl
          }
        }
      """
  }
}
