package io.koff.tf.context_passing

import cats.data.Kleisli
import cats.effect.{IO, IOApp}
import cats.{Applicative, Monad, MonadError, MonadThrow}
import cats.effect.std.Console
import cats.mtl.Ask
import cats.syntax.flatMap.*
import cats.syntax.functor.*

trait Example {

  /** Alternatives for context passing.
    *
    * There are several alternative methods for context passing in Scala:
    *   - Java's `ThreadLocal`
    *   - [[https://typelevel.org/cats-effect/docs/core/io-local IOLocal from cats-effect]]
    *   - [[https://zio.dev/reference/state-management/fiberref/ FiberRef from ZIO]]
    */
  object RenderDoc

  /** First of all, let's define our types that we are going to use in this example.
    *
    *   - `Context` - is a representation of the context we are going to pass across our calls'
    *     stack.
    *   - `Input` - is a type for input params. For simplicity let's use this one type for all
    *     inputs.
    *   - `Output` - is the same but for outputs of our functions.
    */
  type Context
  type Input
  type Output

  /** The next step is to define our services. For demonstration purposes I am going to use these
    * simple services' interfaces. Each one of them contains a single operation that is used by
    * overlying services - `Layer1` uses `Layer2`, `Layer2` uses `Layer3` and so on.
    */
  trait Layer1[F[_]]:
    def operation1(in: Input): F[Output]

  trait Layer2[F[_]]:
    def operation2(in: Input): F[Output]

  trait Layer3[F[_]]:
    def operation3(in: Input): F[Output]

  trait Layer4[F[_]]:
    def operation4(in: Input): F[Output]

  /** Now we need to bring a type class from cats-mtl - `Ask[F[_], Ctx]`. It makes possible of
    * getting a value of `Ctx` out from `F[_]`. To improve readability we can define such a type
    * alias that we can use later a single argument type class.
    */
  import cats.mtl.Ask
  final type AskCtx[F[_]] = Ask[F, Context]

  final class Impl1[F[_]](layer2: Layer2[F]) extends Layer1[F]:
    override def operation1(in: Input): F[Output] = layer2.operation2(in)

  final class Impl2[F[_]](
      verify: Context => F[Boolean],
      layer3: Layer3[F]
  )(using A: AskCtx[F], MT: MonadError[F, Throwable])
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

  final class Impl4[F[_]: Monad](doRealWork: Input => F[Output], println: String => F[Unit])(using
      A: AskCtx[F]
  ) extends Layer4[F]:
    override def operation4(in: Input): F[Output] = for
      context <- A.ask
      output  <- doRealWork(in)
      _       <- println(s"{Context: $context} -> (Input: $in) -> (Output: $output)")
    yield output
}

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

import zio.{Console as ZConsole, *}
object ZIOExample extends ZIOAppDefault with Example:
  import zio.ZIO
  import zio.interop.catz.*
  import zio.interop.catz.mtl.*
  final case class User(name: String)
  override type Context = User
  override type Input   = String
  override type Output  = Int

  private type CtxIO[A] = ZIO[Context, Throwable, A]
  private def doRealWork(in: Input): CtxIO[Output] = Exit.succeed(in.length)
  private def println(in: String): CtxIO[Unit]     = ZConsole.printLine(in)
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
      _ <- ZConsole.print("Let's go!")
      ctxProgram = layer1.operation1(input)
//      result <- ctxProgram.provide(ZLayer.succeed(validCtx))
      result <- ctxProgram.provide(ZLayer.succeed(invalidCtx))
      _      <- ZConsole.printLine(s"result: $result")
    yield ()

/** ==Links==
  *   - [cats-mtl](https://typelevel.org/cats-mtl)
  *   - [Ask typeclass](https://typelevel.org/cats-mtl/mtl-classes/ask.html)
  */
