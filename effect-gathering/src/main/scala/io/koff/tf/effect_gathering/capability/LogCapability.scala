package io.koff.tf.effect_gathering.capability

/** This interface defines an additional layer of abstraction over logging. It can be implemented in
  * two ways:
  *   - straight forward logging using Sync-based impl.
  *   - implementation based on WriterT
  */
trait LogCapability[F[_]] {
  def info(tag: String, input: String, output: String): F[Unit]
  def error(tag: String, input: String, error: Throwable): F[Unit]
}

object LogCapability {
  import cats.effect.std.Console

  /** Writes log messages straight to the console */
  private final class ConsoleLogCapability[F[_]: Console] extends LogCapability[F]:
    def info(tag: String, input: String, output: String): F[Unit] =
      Console[F].println(s"Info[tag:$tag; input:$input; output:$output]")
    def error(tag: String, input: String, error: Throwable): F[Unit] =
      Console[F].println(s"Error[tag:$tag; input:$input; error:$error]")

  import io.koff.tf.effect_gathering.BasicTypes.TellLogs
  import io.koff.tf.effect_gathering.Log

  /** Collects logs into the F[_] */
  private final class TellBasedLogCapability[F[_]](using T: TellLogs[F]) extends LogCapability[F]:
    def info(tag: String, input: String, output: String): F[Unit] =
      T.tell(Log.Info(tag, input, output))
    def error(tag: String, input: String, error: Throwable): F[Unit] =
      T.tell(Log.Error(tag, input, error))

  def console[F[_]: Console]: LogCapability[F]  = new ConsoleLogCapability[F]
  def writerT[F[_]: TellLogs]: LogCapability[F] = new TellBasedLogCapability[F]

  def apply[F[_]: LogCapability]: LogCapability[F] = summon[LogCapability[F]]
}
