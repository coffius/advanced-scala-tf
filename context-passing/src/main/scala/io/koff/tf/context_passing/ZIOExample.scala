package io.koff.tf.context_passing

import zio.*
import zio.interop.catz.mtl.*
import zio.interop.catz.*

object ZIOExample extends ZIOAppDefault with Example:
  final case class User(name: String)
  override type Context = User
  override type Input   = String
  override type Output  = Int

  private type CtxIO[A] = ZIO[Context, Throwable, A]
  private def doRealWork(in: Input): CtxIO[Output] =
    Exit.succeed(in.length)
  private def println(in: String): CtxIO[Unit] =
    Console.printLine(in)
  private def verify(ctx: Context): CtxIO[Boolean] =
    Exit.succeed(ctx.name.nonEmpty)

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
      // Construct a program that requires a context value
      ctxProgram = layer1.operation1(input)
      // Pass the context value to run the program
      result <- ctxProgram.provide(ZLayer.succeed(validCtx))
//    result <- ctxProgram.provide(ZLayer.succeed(invalidCtx))
      _ <- Console.printLine(s"result: $result")
    yield ()
