package io.koff.tf.effect_gathering

import cats.Applicative
import cats.data.Chain
import cats.effect.std.Console
import cats.effect.{IO, IOApp}
import io.koff.tf.effect_gathering.BasicTypes.REff
import io.koff.tf.effect_gathering.TellExtension.TellForChain

object CatsExample extends Example1 with IOApp.Simple {
  override def run: IO[Unit] =
    val lowLvlOp1: Input1 => REff[Output1]  = s => Applicative[REff].pure(s.length)
    val lowLvlOp2: Output1 => REff[Output2] = i => Applicative[REff].pure(i.toDouble)
    val lowLvlOp3: Input2 => REff[Output1]  = l => Applicative[REff].pure(l.size)

    val service: Service[REff] = ServiceImpl[REff](lowLvlOp1, lowLvlOp2, lowLvlOp3)

    val program = for
      result1 <- service.operation1("hello")
      result2 <- service.operation2("hello".toList)
    yield (result1, result2)

    for
      _                   <- Console[IO].println("Hello Cats")
      (logs, resultOrErr) <- program.run
      _                   <- LogProcessor.processLogs[IO, Chain](logs)
      (r1, r2)            <- IO.fromEither(resultOrErr)
      _                   <- Console[IO].println(s"result1: $r1")
      _                   <- Console[IO].println(s"result2: $r2")
    yield ()
}
