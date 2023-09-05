package io.koff.tf.context_passing

import cats.data.Kleisli
import cats.effect.{IO, IOApp}
import cats.{Monad, MonadThrow}
import cats.mtl.Ask
import cats.effect.std.Console
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import io.koff.tf.context_passing.Example.Impl4

trait Example {
  type Context
  type Input
  type Output

  final type AskCtx[F[_]] = Ask[F, Context]

  trait Layer1[F[_]]:
    def operation1(in: Input): F[Output]

  trait Layer2[F[_]]:
    def operation2(in: Input): F[Output]

  trait Layer3[F[_]]:
    def operation3(in: Input): F[Output]

  trait Layer4[F[_]]:
    def operation4(in: Input): F[Output]

  final class Impl1[F[_]](layer2: Layer2[F]) extends Layer1[F]:
    override def operation1(in: Input): F[Output] = layer2.operation2(in)

  final class Impl2[F[_]](
      verify: Context => F[Boolean],
      layer3: Layer3[F]
  )(using A: AskCtx[F], MT: MonadThrow[F])
      extends Layer2[F]:
    override def operation2(in: Input): F[Output] = for {
      context <- A.ask
      result  <- verify(context)
      output <-
        if (result)
          layer3.operation3(in)
        else
          // Better way of working with errors is going to be shown in the `error-handling` part
          MT.raiseError(new RuntimeException(s"Could not verify context: $context"))
    } yield output

  final class Impl3[F[_]](layer4: Layer4[F]) extends Layer3[F]:
    override def operation3(in: Input): F[Output] = layer4.operation4(in)

  final class Impl4[F[_]: Monad](doRealWork: Input => F[Output])(using A: AskCtx[F], C: Console[F])
      extends Layer4[F]:
    override def operation4(in: Input): F[Output] = for
      context <- A.ask
      output  <- doRealWork(in)
      _       <- C.println(s"{Context: $context} -> (Input: $in) -> (Output: $output)")
    yield output
}

object Example extends IOApp.Simple with Example:
  final case class User(name: String)
  override type Context = User
  override type Input   = String
  override type Output  = Int

  private type CtxIO[A] = Kleisli[IO, Context, A]
  private def doRealWork(in: Input): CtxIO[Output] = in.length.pure
  private def verify(ctx: Context): CtxIO[Boolean] = ctx.name.nonEmpty.pure
  override def run: IO[Unit] =
    // type definitions are for clarity only
    val layer4: Layer4[CtxIO] = Impl4(doRealWork)
    val layer3: Layer3[CtxIO] = Impl3(layer4)
    val layer2: Layer2[CtxIO] = Impl2(verify, layer3)
    val layer1: Layer1[CtxIO] = Impl1(layer2)

    val validCtx: Context   = User("valid_name")
    val invalidCtx: Context = User("")
    val input: Input        = "non_empty_string"

    for
      _ <- Console[IO].println("Let's go!")
      ctxProgram = layer1.operation1(input)
      result <- ctxProgram.run(validCtx)
//      result <- ctxProgram.run(invalidCtx)
      _ <- Console[IO].println(s"result: $result")
    yield ()
