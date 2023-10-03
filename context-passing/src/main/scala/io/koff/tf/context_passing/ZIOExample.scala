package io.koff.tf.context_passing

import cats.Applicative
import cats.mtl.Ask
import zio.*

object ZIOExample extends ZIOAppDefault with Example:
  import zio.ZIO
  import zio.interop.catz.*
  final case class User(name: String)
  override type Context = User
  override type Input   = String
  override type Output  = Int

  private type CtxIO[A] = ZIO[Context, Throwable, A]
  private def doRealWork(in: Input): CtxIO[Output] = Exit.succeed(in.length)
  private def println(in: String): CtxIO[Unit]     = Console.printLine(in)
  private def verify(ctx: Context): CtxIO[Boolean] = Exit.succeed(ctx.name.nonEmpty)

  implicit def zioAsk[R1: Tag, R <: R1, E](implicit
      ev: Applicative[ZIO[R, E, _]]
  ): Ask[ZIO[R, E, _], R1] =
    new Ask[ZIO[R, E, _], R1] {
      override def applicative: Applicative[ZIO[R, E, _]] = ev
      override def ask[R2 >: R1]: ZIO[R, E, R2]           = ZIO.environment[R1].map(_.get)
    }
  override def run: ZIO[Any & ZIOAppArgs & Scope, Any, Any] =
    // type definitions are here for clarity only
    val layer4: Layer4[CtxIO] = Impl4[CtxIO](doRealWork, println)
    val layer3: Layer3[CtxIO] = Impl3(layer4)
    val layer2: Layer2[CtxIO] = Impl2(verify, layer3)
    val layer1: Layer1[CtxIO] = Impl1(layer2)

    val validCtx: Context   = User("valid_name")
    val invalidCtx: Context = User("")
    val input: Input        = "non_empty_string"

    for
      _ <- Console.print("Let's go!")
      ctxProgram = layer1.operation1(input)
      // result <- ctxProgram.provide(ZLayer.succeed(validCtx))
      result <- ctxProgram.provide(ZLayer.succeed(invalidCtx))
      _      <- Console.printLine(s"result: $result")
    yield ()
