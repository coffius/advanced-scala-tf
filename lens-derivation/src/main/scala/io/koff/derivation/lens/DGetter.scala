package io.koff.derivation.lens

import monocle.Getter

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
  def fromGetter[S, A](getter: Getter[S, A]): DGetter[S, A] = (s: S) => getter.get(s)
}
