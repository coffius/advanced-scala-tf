package io.koff.tf.effect_gathering

sealed trait EvtLog[+Evt]
object EvtLog {
  import cats.data.NonEmptyList as NeColl
  case object Empty extends EvtLog[Nothing]
  type Empty = Empty.type
  final case class NotEmpty[Evt](values: NeColl[Evt]) extends EvtLog[Evt]
}

import cats.data.{AndThen, Chain, IndexedStateT, NonEmptyList as NeColl}
import io.koff.tf.effect_gathering.EffState.EffSuccess
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

sealed trait EffState[Err, -SA, +SB, +A] {
  def flatMap[B, SC](
      fas: A => EffState[Err, SB, SC, B]
  ): EffState[Err, SA, SC, B] = {
    this match
      case EffState.EffSuccess(run) =>
        EffSuccess { sa =>
          val (sb, a)                       = run(sa)
          val res: EffState[Err, SB, SC, B] = fas(a)
          res match
            case EffSuccess(run)                   => run(sb)
            case EffState.EffError(err, lastState) => ???
        }
      case EffState.EffError(err, lastState) => EffState.EffError(err, lastState)
  }
}
object EffState {
  final case class EffSuccess[SA, SB, A](run: SA => (SB, A)) extends EffState[Nothing, SA, SB, A]
  final case class EffError[Err, S](err: Err, lastState: S)
      extends EffState[Err, S, Nothing, Nothing]
}

sealed trait BusinessEff[Log, Evt, Err, A]

object BusinessEff {
  final case class Pure[Log, Evt, Err, A](log: Log, either: Either[Err, (Evt, A)])
      extends BusinessEff[Log, Evt, Err, A]
  final case class Map[Log, Evt, Err, A, B](f: A => B) extends BusinessEff[Log, Evt, Err, A]
  final case class FlatMap[Log, Evt, Err, A, B](f: A => BusinessEff[Log, Evt, Err, B])
      extends BusinessEff[Log, Evt, Err, A]
}
