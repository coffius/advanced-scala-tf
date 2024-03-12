package io.koff.tf.effect_gathering

import cats.effect.std.Console as CatsConsole
import cats.mtl.Tell
import cats.syntax.show.*
import cats.{Functor, Monad, Show}
import zio.*
import zio.prelude.fx.ZPure

import java.nio.charset.Charset

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

      def tell(l: W): ZPure[W, S, S, R, E, Unit] = ZPure.log(l)
}
