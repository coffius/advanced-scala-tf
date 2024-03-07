package io.koff.tf.effect_gathering

import cats.Traverse
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.effect.std.Console

object LogProcessor {
  def processLogs[F[_]: Console, Col[_]: Traverse](logs: Col[Log]): F[Unit] =
    val logsAsStrings = logs.map:
      case Log.Info(tag, input, output) =>
        s"Log[level = INFO; tag = $tag; input = $input; output = $output]"
      case Log.Error(tag, input, error) =>
        s"Log[level = ERROR; tag = $tag; input = $input; error = $error]"
    val logsAsString = logsAsStrings.mkString_("\n")
    Console[F].println(logsAsString)
}
