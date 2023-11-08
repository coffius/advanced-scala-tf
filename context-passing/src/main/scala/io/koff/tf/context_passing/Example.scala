package io.koff.tf.context_passing

import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.{Monad, MonadError}

trait Example {

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
          // Better way of working with errors
          // is going to be shown in the `error-handling` part
          MT.raiseError(new RuntimeException(s"Could not verify context: $context"))
    } yield output

  final class Impl3[F[_]](layer4: Layer4[F]) extends Layer3[F]:
    override def operation3(in: Input): F[Output] = layer4.operation4(in)

  final class Impl4[F[_]: Monad](
      doRealWork: Input => F[Output],
      println: String => F[Unit]
  )(using
      A: AskCtx[F]
  ) extends Layer4[F]:
    override def operation4(in: Input): F[Output] = for
      context <- A.ask
      output  <- doRealWork(in)
      _       <- println(s"{Context: $context} -> (Input: $in) -> (Output: $output)")
    yield output
}

/** ==Links==
  *   - [cats-mtl](https://typelevel.org/cats-mtl)
  *   - [Ask typeclass](https://typelevel.org/cats-mtl/mtl-classes/ask.html)
  */
