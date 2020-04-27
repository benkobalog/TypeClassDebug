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

    def extractClassParams(tpe: Type): List[MethodSymbol] =
      tpe.members.collect {
        case m: MethodSymbol if m.isCaseAccessor => m
      }.toList

    def constructInstanceTree(currentType: MethodSymbol): TypeClassInstanceTree = {
      val currentTypeName = currentType.returnType.toString
      try {
        c.typecheck(c.parse(s"implicitly[$typeClassTypeName[$currentTypeName]]"))
        ExistingInstance
      } catch {
        case _: TypecheckException =>
          val classParams = extractClassParams(currentType.returnType)
          val fieldName = currentType.name.toString
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
