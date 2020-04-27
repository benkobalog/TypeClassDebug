package typeclassdebug

sealed trait TypeClassInstanceTree

case class ParentOfMissing(fieldName: String, branches: List[TypeClassInstanceTree]) extends TypeClassInstanceTree

case class MissingInstance(fieldName: String, typeName: String) extends TypeClassInstanceTree {
  override def toString: String = s"$fieldName: [$typeName]"
}

case object ExistingInstance extends TypeClassInstanceTree

object TypeClassInstanceTree {
  def collectMissingInstances(tree: TypeClassInstanceTree): List[MissingInstance] =
    tree match {
      case ExistingInstance => Nil
      case m: MissingInstance => List(m)
      case ParentOfMissing(parentName, branches) =>
        branches
          .flatMap(collectMissingInstances)
          .map { missingInstance =>
            missingInstance.copy(fieldName = s"$parentName.${missingInstance.fieldName}")
          }
    }
}