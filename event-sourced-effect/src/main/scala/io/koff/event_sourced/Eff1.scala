package io.koff.event_sourced

import cats.data.{NonEmptyList as NeColl, *}

sealed trait Eff1[Ef, +Evt, Err, A]:
  def flatMap[Evt1 >: Evt, B](f: A => Eff1[Ef, Evt1, Err, B]): Eff1[Ef, Evt1, Err, B] = this match
    case Eff1.Empty(effLog1, either) =>
      either match
        case Left(error) => Eff1.Empty(effLog1, Left(error))
        case Right(a) =>
          f(a) match
            case Eff1.Empty(effLog2, either)       => Eff1.Empty(effLog1 ++ effLog2, either)
            case Eff1.NonEmpty(effLog2, events, b) => Eff1.NonEmpty(effLog1 ++ effLog2, events, b)
    case Eff1.NonEmpty(effLog1, events1, a) =>
      f(a) match
        case Eff1.Empty(effLog2, either) =>
          either match
            case Left(error) => Eff1.Empty(effLog1 ++ effLog2, Left(error))
            case Right(b)    => Eff1.NonEmpty(effLog1 ++ effLog2, events1, b)
        case Eff1.NonEmpty(effLog2, events2, b) =>
          Eff1.NonEmpty(effLog1 ++ effLog2, events1.concatNel(events2), b)

object Eff1:
  final case class Empty[Ef, Err, A](effLog: Chain[Ef], either: Either[Err, A])
      extends Eff1[Ef, Nothing, Err, A]
  final case class NonEmpty[Ef, Evt, Err, A](
      effLog: Chain[Ef],
      events: NeColl[Evt],
      value: A
  ) extends Eff1[Ef, Evt, Err, A]
