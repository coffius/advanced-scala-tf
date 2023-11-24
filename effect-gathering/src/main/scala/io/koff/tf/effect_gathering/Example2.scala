package io.koff.tf.effect_gathering

import cats.Monad
import cats.effect.std.Console
import cats.effect.{IO, IOApp}
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.show.*
import io.koff.tf.effect_gathering.BasicTypes.*
import io.koff.tf.effect_gathering.TellExtension.*

trait Example2 {

  /** This is a logger defined as capability. Pay attention that its methods as equivalent to
    * the `TagLog` structure.
    */
  trait Log[F[_]]:
    def info(tag: String, input: String, output: String): F[Unit]
    def error(tag: String, input: String, error: Throwable): F[Unit]

  /** This trait can be implemented in two ways. */
  /** An usual implementation - when logs are sent to the outside world when an effect is executed. */
  protected final class ConsoleTagLog[F[_]: Console] extends Log[F]:
    override def info(tag: String, input: String, output: String): F[Unit] =
      Console[F].println(s"INFO - tag: $tag; input: $input; output: $output")
    override def error(tag: String, input: String, error: Throwable): F[Unit] =
      Console[F].println(s"ERROR - tag: $tag; input: $input; error: $error")

  /** Another way is to put our logs into the F[_] effect using the Tell[..] type class. */
  protected final class InEffectTagLog[F[_]: TellTagLogs] extends Log[F]:
    override def info(tag: String, input: String, output: String): F[Unit] =
      Log.Info(tag, input, output).tell[F]

    override def error(tag: String, input: String, error: Throwable): F[Unit] =
      Log.Error(tag, input, error).tell[F]

  /** Now let's take out previous service and use this Log[..] capability */
  trait Service2[F[_]]:
    def operation1(in: Input1): F[Output1]
    def operation2(in: Input2): F[Output2]

  protected final class Service2Impl[F[_]: Monad](
      lowLvlOp1: Input1 => F[Output1],
      lowLvlOp2: Output1 => F[Output2],
      lowLvlOp3: Input2 => F[Output1]
  )(using
      L: Log[F]
  ) extends Service2[F]:
    override def operation1(in: Input1): F[Output1] = for
      out1 <- lowLvlOp1(in)
      // So here we use our `L: Log[F]` instead of Tell[..]
      _    <- L.info("operation1", in.show, out1.show)
      out2 <- lowLvlOp2(out1)
      _    <- L.info("operation1", in.show, out2.show)
    yield out1

    override def operation2(in: Input2): F[Output2] = for
      out1 <- lowLvlOp3(in)
      // and here as well
      _    <- L.info("operation2", in.show, out1.show)
      out2 <- lowLvlOp2(out1)
      _    <- L.info("operation2", in.show, out2.show)
    yield out2
}
object Example2 extends Example2 with IOApp.Simple:
  private def program[F[_]: Log: Monad]: F[(Output1, Output2)] =
    val lowLvlOp1: Input1 => F[Output1]  = s => s.length.pure
    val lowLvlOp2: Output1 => F[Output2] = i => i.toDouble.pure
    val lowLvlOp3: Input2 => F[Output1]  = l => l.size.pure

    val service: Service2[F] = Service2Impl[F](lowLvlOp1, lowLvlOp2, lowLvlOp3)

    for
      result1 <- service.operation1("hello")
      result2 <- service.operation2("hello".toList)
    yield (result1, result2)

  private given Log[IO]  = new ConsoleTagLog[IO]
  private given Log[Eff] = new InEffectTagLog[Eff]
  override def run: IO[Unit] = for
    _ <- Console[IO].println("Hello ConsoleTagLog")
    // Run the program using a console impl of `Log[..]`
    (res1, res2) <- program[IO]
    _            <- Console[IO].println(s"console result1: $res1")
    _            <- Console[IO].println(s"console result2: $res2")
    _            <- Console[IO].println("")
    _            <- Console[IO].println("Hello InEffectTagLog")
    // Now let's run the same program but with gathering logs in `Eff[..]`
    (logs, (res1, res2)) <- program[Eff].run
    _                    <- Console[IO].println(s"logs: $logs")
    _                    <- Console[IO].println(s"result1: $res1")
    _                    <- Console[IO].println(s"result2: $res2")
  yield ()
