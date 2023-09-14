package io.koff.tf.effect_gathering

import cats.data.{Chain, WriterT}
import cats.effect.IOApp
import cats.{Monad, Show}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.show.*
import io.koff.tf.effect_gathering.TellExtension.*
import BasicTypes.*

trait Example1 {

  /** Example of half structured log - its cases have additional info. We are going to use it as a
    * data structure for gathering info needed to be logged.
    */

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
      // Here we see how log instances can be added to F[_]
      _    <- tellOne(TagLog.Info("operation1", in.show, out1.show))
      out2 <- lowLvlOp2(out1)
      _    <- tellOne(TagLog.Info("operation1", in.show, out2.show))
    yield out1

    override def operation2(in: Input2): F[Output2] = for
      out1 <- lowLvlOp3(in)
      // and here as well
      _    <- TagLog.Info("operation2", in.show, out1.show).tell[F]
      out2 <- lowLvlOp2(out1)
      _    <- TagLog.Info("operation2", in.show, out2.show).tell[F]
    yield out2
}

object Example1 extends Example1 with IOApp.Simple:
  import cats.effect.IO
  import cats.effect.std.Console

  override def run: IO[Unit] =
    val lowLvlOp1: Input1 => Eff[Output1]  = s => WriterT.liftF(IO.pure(s.length))
    val lowLvlOp2: Output1 => Eff[Output2] = i => WriterT.liftF(IO.pure(i.toDouble))
    val lowLvlOp3: Input2 => Eff[Output1]  = l => WriterT.liftF(IO.pure(l.size))

    val service: Service[Eff] = ServiceImpl(lowLvlOp1, lowLvlOp2, lowLvlOp3)

    val program = for
      result1 <- service.operation1("hello")
      result2 <- service.operation2("hello".toList)
    yield (result1, result2)

    for
      _                    <- Console[IO].println("Hello WriterT")
      (logs, (res1, res2)) <- program.run
      _                    <- Console[IO].println(s"logs: $logs")
      _                    <- Console[IO].println(s"result1: $res1")
      _                    <- Console[IO].println(s"result2: $res2")
    yield ()
