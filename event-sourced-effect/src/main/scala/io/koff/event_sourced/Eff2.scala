package io.koff.event_sourced

import cats.data.Chain

final case class Eff2[+Ef, +Err, SA, SB, A](run: SA => (Chain[Ef], Either[Err, (SB, A)])) {
  def flatMap[Ef1 >: Ef, Err1 >: Err, SC, B](
      f: A => Eff2[Ef1, Err1, SB, SC, B]
  ): Eff2[Ef1, Err1, SA, SC, B] =
    Eff2 { sa =>
      val (chain1, either) = run(sa)
      either match {
        case Left(error) => (chain1, Left(error)) // fail fast
        case Right((sb, a)) =>
          val nextRun          = f(a)
          val (chain2, either) = nextRun.run(sb)
          either match {
            case Left(error)  => (chain1 ++ chain2, Left(error)) // combine effects and fail next
            case Right(value) => (chain1 ++ chain2, Right(value))
          }
      }
    }

  def map[B](f: A => B): Eff2[Ef, Err, SA, SB, B] = Eff2 { sa =>
    val (chain, either) = run(sa)
    either match {
      case Left(error)    => (chain, Left(error)) // fail fast
      case Right((sb, a)) => (chain, Right((sb, f(a))))
    }
  }
}

object Eff2 {
  def pure[S, A](a: A): Eff2[Nothing, Nothing, S, S, A] = Eff2(s => (Chain.nil, Right((s, a))))
  def fromEither[S, L, R](either: Either[L, R]): Eff2[Nothing, L, S, S, R] = either match
    case Left(left)   => Eff2(_ => (Chain.nil, Left(left)))
    case Right(right) => Eff2(s => (Chain.nil, Right((s, right))))
}
