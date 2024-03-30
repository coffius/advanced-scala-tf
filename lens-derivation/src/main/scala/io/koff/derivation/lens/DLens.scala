package io.koff.derivation.lens

trait DLens[S, A] {
  self =>
  def get(s: S): A
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
