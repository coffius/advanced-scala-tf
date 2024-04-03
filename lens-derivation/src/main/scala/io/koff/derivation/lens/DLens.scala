package io.koff.derivation.lens

import io.koff.derivation.lens.compile.NonEmptyProductOf

import scala.Tuple.{Head, Tail}
import scala.deriving.Mirror

/** Derivable Getter */
trait DGetter[S, A] {
  self =>
  def get(s: S): A

  def andThen[B](next: DGetter[A, B]): DGetter[S, B] =
    (s: S) =>
      val a = self.get(s)
      next.get(a)
}

object DGetter {
  def id[A]: DGetter[A, A] = new DGetter[A, A]:
    override def get(s: A): A = s

  inline given emptyTupleGetter[S]: DGetter[S, EmptyTuple] = new DGetter[S, EmptyTuple]:
    override def get(s: S): EmptyTuple = EmptyTuple
  inline given tupleHeadLens[S](using
      m: NonEmptyProductOf[S]
  ): DGetter[S, Head[m.MirroredElemTypes]] = new DGetter[S, Head[m.MirroredElemTypes]]:
    override def get(s: S): Head[m.MirroredElemTypes] = m.convert(s).head

  given tupleTailLens[S](using m: NonEmptyProductOf[S]): DGetter[S, Tail[m.MirroredElemTypes]] =
    (s: S) => m.convert(s).tail

}

/** Derivable Lens */
trait DLens[S, A] extends DGetter[S, A] {
  self =>
  def modify(s: S, a: A): S

  def andThen[B](other: DLens[A, B]): DLens[S, B] = new DLens[S, B]:
    override def get(s: S): B = other.get(self.get(s))
    override def modify(s: S, b: B): S =
      val newA = other.modify(self.get(s), b)
      self.modify(s, newA)
}

object DLens {
  implicit def id[A]: DLens[A, A] = new DLens[A, A]:
    override def get(s: A): A          = s
    override def modify(s: A, a: A): A = a
}
