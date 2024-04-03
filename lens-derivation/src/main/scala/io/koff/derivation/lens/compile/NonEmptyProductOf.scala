package io.koff.derivation.lens.compile

import cats.NonEmptyTraverse
import io.koff.derivation.lens.User

import scala.Tuple.{Head, Tail}
import scala.compiletime.*
import scala.deriving.Mirror

trait NonEmptyProductOf[V] {
  type MirroredType     = V
  type MirroredMonoType = V
  type MirroredElemTypes <: NonEmptyTuple

  def convert(t: V): MirroredElemTypes
}

object NonEmptyProductOf {
  private type AsNonEmpty[T] = Mirror.Product {
    type MirroredType     = T
    type MirroredMonoType = T
    type MirroredElemTypes <: NonEmptyTuple
  }
  inline given nonEmptyProductOf[S <: Product](using m: Mirror.ProductOf[S]): NonEmptyProductOf[S] =
    inline erasedValue[m.MirroredElemTypes] match
      case _: NonEmptyTuple =>
        val asNonEmpty = m.asInstanceOf[AsNonEmpty[S]]
        new NonEmptyProductOf[S] {
          override type MirroredElemTypes = asNonEmpty.MirroredElemTypes
          override def convert(t: S): asNonEmpty.MirroredElemTypes =
            Tuple.fromProductTyped(t).asInstanceOf[MirroredElemTypes]
        }
      case _: EmptyTuple => error("can't derive NonEmptyProductOf[T] for an empty product")

}
