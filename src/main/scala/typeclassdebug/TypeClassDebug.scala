package typeclassdebug

import scala.language.experimental.macros
import scala.reflect.macros.{TypecheckException, blackbox}

object TypeClassDebug {

  def findMissingInstances[TypeClass[_], CurrentType]: Nothing = macro findMissingInstancesImpl[TypeClass, CurrentType]

  def findMissingInstancesImpl[TypeClass[_], CurrentType](c: blackbox.Context)(
    implicit currentType: c.WeakTypeTag[CurrentType], typeClassType: c.WeakTypeTag[TypeClass[CurrentType]]
  ): c.Expr[Nothing] = {
    import c.universe._
    val typeClassTypeName = typeClassType.tpe.toString

    case class Field(tpe: Type, name: String)

    def extractClassParams(tpe: Type): List[Field] =
      tpe.members.collect { case param: MethodSymbol if param.isCaseAccessor => param }
        .toList
        .flatMap { param =>
          // I added the typeArgs part to handle Option and Either
          val typeArgs = param.returnType.typeArgs
          if (typeArgs.isEmpty) List(Field(param.returnType, param.name.toString))
          else typeArgs.map(t => Field(t, param.name.toString))
        }

    def constructInstanceTree(field: Field): TypeClassInstanceTree = {
      val currentTypeName = field.tpe.toString
      try {
        c.typecheck(c.parse(s"implicitly[$typeClassTypeName[$currentTypeName]]"))
        ExistingInstance
      } catch {
        case _: TypecheckException =>
          val classParams = extractClassParams(field.tpe)
          val fieldName = field.name
          val childInstances = classParams.map(constructInstanceTree)
          if (childInstances.isEmpty)
            MissingInstance(fieldName, currentTypeName)
          else
            ParentOfMissing(fieldName, childInstances)
      }
    }

    val topLvlClassParams = extractClassParams(currentType.tpe)

    val missingInstances = topLvlClassParams
      .map(constructInstanceTree)
      .flatMap(TypeClassInstanceTree.collectMissingInstances)

    if (missingInstances.nonEmpty) {
      val errorMsg = missingInstances.mkString(s"$typeClassTypeName instance is missing for: \n", "\n", "")
      c.error(c.enclosingPosition, errorMsg)
    }
    c.Expr(q"")
  }
}
