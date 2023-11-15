package io.koff.tf.effect_gathering

// https://github.com/manatki/manatki/blob/master/src/main/scala/manatki/data/ReliableWriterT.scala
import cats.*
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.semigroup.*

/** Replacement for WriterT capable of keeping written value in case of error */
final case class ReliableWriterT[F[_], E, W, A](run: F[(W, Either[E, A])]) {
  def mapAll[W1, E1, A1](f: (W, Either[E, A]) => (W1, Either[E1, A1]))(implicit
      F: Functor[F]
  ): ReliableWriterT[F, E1, W1, A1] =
    ReliableWriterT(F.map(run)(f.tupled))

  def map[B](f: A => B)(implicit F: Functor[F]): ReliableWriterT[F, E, W, B] = mapAll {
    case (w, ea) => (w, ea.map(f))
  }

  def put(w: W)(implicit F: Functor[F]): ReliableWriterT[F, E, W, A] = ReliableWriterT(F.map(run) {
    case (_, either) => (w, either)
  })
  def flatMap[B](
      f: A => ReliableWriterT[F, E, W, B]
  )(implicit W: Semigroup[W], F: MonadError[F, E]): ReliableWriterT[F, E, W, B] =
    ReliableWriterT(run.flatMap {
      case (w, Left(e)) => F.pure((w, Left(e)))
      case (w1, Right(x)) =>
        f(x).run.map { case (w2, eb) => (w1 |+| w2, eb) }.handleError(e => (w1, Left(e)))
    })

  def handleWith(
      f: E => ReliableWriterT[F, E, W, A]
  )(implicit W: Semigroup[W], F: MonadError[F, E]): ReliableWriterT[F, E, W, A] =
    ReliableWriterT(run.flatMap {
      case (w, Right(x)) => F.pure((w, Right(x)))
      case (w1, Left(e)) =>
        f(e).run.map { case (w2, eb) => (w1 |+| w2, eb) }.handleError(e => (w1, Left(e)))
    })
}

object ReliableWriterT {
  implicit def instance[F[_], E, W](implicit
      F: MonadError[F, E],
      W: Monoid[W]
  ): MonadError[ReliableWriterT[F, E, W, *], E] =
    new MonadError[ReliableWriterT[F, E, W, *], E] {
      override def pure[A](x: A): ReliableWriterT[F, E, W, A] = ReliableWriterT(
        F.pure((W.empty, Right(x)))
      )
      override def flatMap[A, B](fa: ReliableWriterT[F, E, W, A])(
          f: A => ReliableWriterT[F, E, W, B]
      ): ReliableWriterT[F, E, W, B] =
        fa.flatMap(f)
      override def tailRecM[A, B](
          init: A
      )(f: A => ReliableWriterT[F, E, W, Either[A, B]]): ReliableWriterT[F, E, W, B] =
        ReliableWriterT(F.tailRecM((W.empty, init)) { case (w1, a) =>
          f(a).run
            .map {
              case (w2, Left(e))         => Right((w1 |+| w2, Left(e)))
              case (w2, Right(Left(a1))) => Left((w1 |+| w2, a1))
              case (w2, Right(Right(b))) => Right((w1 |+| w2, Right(b)))
            }
            .handleError(e => Right((w1, Left(e))))
        })
      override def raiseError[A](e: E): ReliableWriterT[F, E, W, A] = ReliableWriterT(
        F.pure((W.empty, Left(e)))
      )
      override def handleErrorWith[A](fa: ReliableWriterT[F, E, W, A])(
          f: E => ReliableWriterT[F, E, W, A]
      ): ReliableWriterT[F, E, W, A] =
        fa.handleWith(f)
    }

  import cats.mtl.Tell
  implicit def reliableWriterTTell[F[_], E, L](using
      A: Applicative[F]
  ): Tell[ReliableWriterT[F, E, L, *], L] =
    new Tell[ReliableWriterT[F, E, L, *], L] {
      def functor: Functor[ReliableWriterT[F, E, L, *]] =
        new Functor[ReliableWriterT[F, E, L, *]]:
          override def map[A, B](fa: ReliableWriterT[F, E, L, A])(
              f: A => B
          ): ReliableWriterT[F, E, L, B] =
            fa.map(f)

      def tell(l: L): ReliableWriterT[F, E, L, Unit] = ReliableWriterT(A.pure((l, Right(()))))
    }
}
