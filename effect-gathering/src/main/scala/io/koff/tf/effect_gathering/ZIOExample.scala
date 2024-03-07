package io.koff.tf.effect_gathering

import cats.effect.std.Console as CatsConsole
import cats.mtl.Tell
import cats.syntax.show.*
import cats.{Functor, Monad, Show}
import io.koff.tf.effect_gathering.ZPureInstances.given
import zio.interop.catz.*
import zio.prelude.Writer as ZWriter
import zio.prelude.fx.ZPure
import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault, *}

import java.nio.charset.Charset

object ZIOExample extends ZIOAppDefault with Example1:
  type REff[T] = ZWriter[Log, T]
  override def run: ZIO[Any & ZIOAppArgs & Scope, Any, Any] =
    val lowLvlOp1: Input1 => REff[Output1]  = s => ZWriter.succeed(s.length)
    val lowLvlOp2: Output1 => REff[Output2] = i => ZWriter.succeed(i.toDouble)
    val lowLvlOp3: Input2 => REff[Output1]  = l => ZWriter.succeed(l.size)

    val service: Service[REff] = ServiceImpl(lowLvlOp1, lowLvlOp2, lowLvlOp3)

    val program = for
      result1 <- service.operation1("hello")
      result2 <- service.operation2("hello".toList)
    yield (result1, result2)

    for
      _ <- Console.printLine("Let's go!")
      (logs, resultOrErr) = program.runAll(())
      _ <- LogProcessor.processLogs[Task, Chunk](logs)
      _ <- Console.printLine(s"result: $resultOrErr")
    yield ()

object ZPureInstances {
  given CatsConsoleForZIO: CatsConsole[Task] = new CatsConsole[Task]:
    override def readLineWithCharset(charset: Charset): Task[String] = Console.readLine
    override def print[A](a: A)(implicit S: Show[A]): Task[Unit]     = Console.print(a.show)
    override def println[A](a: A)(implicit S: Show[A]): Task[Unit]   = Console.printLine(a.show)
    override def error[A](a: A)(implicit S: Show[A]): Task[Unit]     = Console.printError(a.show)
    override def errorln[A](a: A)(implicit S: Show[A]): Task[Unit] = Console.printLineError(a.show)

  given ZPureMonad[W, S, R, E]: Monad[[X] =>> ZPure[W, S, S, R, E, X]] =
    new Monad[[X] =>> ZPure[W, S, S, R, E, X]]:
      override def flatMap[A, B](fa: ZPure[W, S, S, R, E, A])(
          f: A => ZPure[W, S, S, R, E, B]
      ): ZPure[W, S, S, R, E, B] = fa.flatMap(f)

      override def tailRecM[A, B](a: A)(
          f: A => ZPure[W, S, S, R, E, Either[A, B]]
      ): ZPure[W, S, S, R, E, B] = f(a).flatMap {
        case Left(a)  => tailRecM[A, B](a)(f) // this is an unsafe implementation
        case Right(b) => ZPure.succeed(b)
      }

      override def pure[A](x: A): ZPure[W, S, S, R, E, A] =
        ZPure.succeed(x)

  given ZPureTell[W, S, R, E]: Tell[[X] =>> ZPure[W, S, S, R, E, X], W] =
    new Tell[[X] =>> ZPure[W, S, S, R, E, X], W]:
      def functor: Functor[[X] =>> ZPure[W, S, S, R, E, X]] = ZPureMonad[W, S, R, E]
      def tell(l: W): ZPure[W, S, S, R, E, Unit]            = ZPure.log(l)
}
