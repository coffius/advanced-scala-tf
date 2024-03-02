package io.koff.tf.effect_gathering

import io.koff.tf.effect_gathering.BasicTypes.TellTagLogs
import cats.{Monad, Show}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.show.*
import io.koff.tf.effect_gathering.TellExtension.*

trait Example1 {
  type Input1
  type Input2
  type Output1
  type Output2

  given input1Show: Show[Input1]
  given input2Show: Show[Input2]
  given output1Show: Show[Output1]
  given output2Show: Show[Output2]

  /** This is a basic service with two operations */
  trait Service[F[_]]:
    def operation1(in: Input1): F[Output1]

    def operation2(in: Input2): F[Output2]

  /** This is its implementation. As constructor params it requires two low level operations. In a
    * real system it might be DB operations or http calls to an external system. Also it requires
    * `T: TellAppLogs[F]` which is used to put instances of `AppLog` into `F[_]`
    */
  protected final class ServiceImpl[F[_]: Monad](
      lowLvlOp1: Input1 => F[Output1],
      lowLvlOp2: Output1 => F[Output2],
      lowLvlOp3: Input2 => F[Output1]
  )(using
      T: TellTagLogs[F]
  ) extends Service[F]:
    override def operation1(in: Input1): F[Output1] = for
      out1 <- lowLvlOp1(in)
      // Here we see how log elements can be added to F[_]
      _    <- tellOne(Log.Info("operation1", in.show, out1.show))
      out2 <- lowLvlOp2(out1)
      _    <- tellOne(Log.Info("operation1", in.show, out2.show))
    yield out1

    override def operation2(in: Input2): F[Output2] = for
      out1 <- lowLvlOp3(in)
      // and here as well
      _    <- Log.Info("operation2", in.show, out1.show).tell[F]
      out2 <- lowLvlOp2(out1)
      _    <- Log.Info("operation2", in.show, out2.show).tell[F]
    yield out2
}
