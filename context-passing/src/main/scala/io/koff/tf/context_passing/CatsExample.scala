package io.koff.tf.context_passing

import cats.data.Kleisli
import cats.effect.{IO, IOApp}
import cats.effect.std.Console

object CatsExample extends IOApp.Simple with Example:
  import cats.syntax.applicative.*
  final case class User(name: String)
  override type Context = User
  override type Input   = String
  override type Output  = Int

  private type CtxIO[A] = Kleisli[IO, Context, A]
  private def doRealWork(in: Input): CtxIO[Output] = in.length.pure
  private def println(in: String): CtxIO[Unit]     = Console[CtxIO].println(in)
  private def verify(ctx: Context): CtxIO[Boolean] = ctx.name.nonEmpty.pure
  override def run: IO[Unit] =
    // type definitions are for clarity only
    val layer4: Layer4[CtxIO] = Impl4(doRealWork, println)
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
