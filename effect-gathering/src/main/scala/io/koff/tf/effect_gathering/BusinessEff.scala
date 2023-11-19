package io.koff.tf.effect_gathering

sealed trait EvtLog[+Evt]
object EvtLog {
  import cats.data.NonEmptyList as NeColl
  case object Empty extends EvtLog[Nothing]
  type Empty = Empty.type
  final case class NotEmpty[Evt](values: NeColl[Evt]) extends EvtLog[Evt]
}

import cats.data.{AndThen, Chain, IndexedStateT, NonEmptyList as NeColl}
//type EffState[EvtLog, Err] = Either[Err, EvtLog]
// final case class EffState[Eff, EvtLog, Err](effs: Chain[Eff], either: Either[Err, EvtLog])
// final class IndexedStateT[F[_], SA, SB, A](val runF: F[SA => F[(SB, A)]])

//final class ReliableIndexedState[Err, SA, SB, A](run: SA => (SB, Either[Err, A])) {
//  def flatMap[B, SC](
//      fas: A => ReliableIndexedState[Err, SB, SC, B]
//  ): ReliableIndexedState[Err, SA, SC, B] =
//    ReliableIndexedState.apply({ (sa: SA) =>
//      val (sb, either) = run(sa)
//      either match
//        case Left(err) => (sb, Left(err)) // optimise this!
//        case Right(a)  => fas(a)
//    })
//}

sealed trait Eff[Ef, +Evt, Err, A]:
  def flatMap[Evt1 >: Evt, B](f: A => Eff[Ef, Evt1, Err, B]): Eff[Ef, Evt1, Err, B] = this match
    case Eff.Empty(effLog1, either) =>
      either match
        case Left(error) => Eff.Empty(effLog1, Left(error))
        case Right(a) =>
          f(a) match
            case Eff.Empty(effLog2, either)       => Eff.Empty(effLog1 ++ effLog2, either)
            case Eff.NonEmpty(effLog2, events, b) => Eff.NonEmpty(effLog1 ++ effLog2, events, b)
    case Eff.NonEmpty(effLog1, events1, a) =>
      f(a) match
        case Eff.Empty(effLog2, either) =>
          either match
            case Left(error) => Eff.Empty(effLog1 ++ effLog2, Left(error))
            case Right(b)    => Eff.NonEmpty(effLog1 ++ effLog2, events1, b)
        case Eff.NonEmpty(effLog2, events2, b) =>
          Eff.NonEmpty(effLog1 ++ effLog2, events1.concatNel(events2), b)

object Eff:
  final case class Empty[Ef, Err, A](effLog: Chain[Ef], either: Either[Err, A])
      extends Eff[Ef, Nothing, Err, A]
  final case class NonEmpty[Ef, Evt, Err, A](
      effLog: Chain[Ef],
      events: NeColl[Evt],
      value: A
  ) extends Eff[Ef, Evt, Err, A]
