package io.koff.tf.effect_gathering

import cats.data.{Chain, WriterT}
import cats.effect.IOApp
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.show.*
import io.koff.tf.effect_gathering.TellExtension.*
import BasicTypes.Eff
import cats.Show

object CatsExample extends Example1 with IOApp.Simple:
  import cats.effect.IO
  import cats.effect.std.Console

  override type Input1  = BasicTypes.Input1
  override type Input2  = BasicTypes.Input2
  override type Output1 = BasicTypes.Output1
  override type Output2 = BasicTypes.Output2

  override def input1Show: Show[Input1]   = BasicTypes.input1Show
  override def input2Show: Show[Input2]   = BasicTypes.input2Show
  override def output1Show: Show[Output1] = BasicTypes.output1Show
  override def output2Show: Show[Output2] = BasicTypes.output2Show

  override def run: IO[Unit] =
    val lowLvlOp1: Input1 => Eff[Output1]  = s => WriterT.liftF(IO.pure(s.length))
    val lowLvlOp2: Output1 => Eff[Output2] = i => WriterT.liftF(IO.pure(i.toDouble))
    val lowLvlOp3: Input2 => Eff[Output1]  = l => WriterT.liftF(IO.pure(l.size))

    val service: Service[Eff] = ServiceImpl(lowLvlOp1, lowLvlOp2, lowLvlOp3)

    val program = for
      result1 <- service.operation1("hello")
      result2 <- service.operation2("hello".toList)
    yield (result1, result2)

    for
      _                    <- Console[IO].println("Hello WriterT")
      (logs, (res1, res2)) <- program.run
      _                    <- Console[IO].println(s"logs: $logs")
      _                    <- Console[IO].println(s"result1: $res1")
      _                    <- Console[IO].println(s"result2: $res2")
    yield ()
