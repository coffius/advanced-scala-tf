package io.koff.tf.effect_gathering

import cats.{Applicative, Monoid}
import cats.data.Chain
import cats.mtl.Tell

object TellExtension:
  def tellOne[F[_], Coll[_], L](
      a: L
  )(using T: Tell[F, Coll[L]], A: Applicative[Coll], M: Monoid[Coll[L]]): F[Unit] =
    val collOne = A.pure(a)
    T.tell(collOne)

  extension [L](l: L)
    def 
    tell[F[_]](using Tell[F, Chain[L]]): F[Unit] =
      tellOne(l)
