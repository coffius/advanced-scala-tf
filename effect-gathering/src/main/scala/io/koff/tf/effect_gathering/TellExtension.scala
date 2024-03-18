package io.koff.tf.effect_gathering

import cats.Functor
import cats.data.Chain
import cats.mtl.Tell

object TellExtension {
  given TellForChain[F[_], L](using T: Tell[F, Chain[L]]): Tell[F, L] =
    new Tell[F, L]:
      def functor: Functor[F] = T.functor
      def tell(l: L): F[Unit] = T.tell(Chain.one(l))

  extension [L](l: L)
    def tell[F[_]](using T: Tell[F, L]): F[Unit] =
      T.tell(l)
}
