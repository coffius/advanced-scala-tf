package io.koff.derivation.lens.compile

import scala.quoted.{Expr, Quotes, Type}

object Macros {
  inline def showType[T <: AnyKind]: String = ${ showTypeMacro[T] }
  private def showTypeMacro[T <: AnyKind: Type](using q: Quotes): Expr[String] =
    import q.reflect.*
    Expr(TypeRepr.of[T].dealias.widen.show)
}
