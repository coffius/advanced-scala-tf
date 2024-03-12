package io.koff.tf.effect_gathering

import io.koff.tf.effect_gathering.ZPureInstances.given
import zio.*
import zio.interop.catz.*
import zio.prelude.Writer

object ZIOExample extends ZIOAppDefault with Example1 {
  type REff[T] = Writer[Log, T]

  override def run: ZIO[Any & ZIOAppArgs & Scope, Any, Any] =
    val lowLvlOp1: Input1 => REff[Output1] = s => Writer.succeed(s.length)
    val lowLvlOp2: Output1 => REff[Output2] = i => Writer.succeed(i.toDouble)
    val lowLvlOp3: Input2 => REff[Output1] = l => Writer.succeed(l.size)

    val service: Service[REff] = ServiceImpl(lowLvlOp1, lowLvlOp2, lowLvlOp3)

    val program = for
      result1 <- service.operation1("hello")
      result2 <- service.operation2("hello".toList)
    yield (result1, result2)

    for
      _ <- Console.printLine("Let's go!")
      (logs, resultOrErr) = program.runAll(())
      _ <- LogProcessor.processLogs[Task, Chunk](logs)
      _ <- Console.printLine(s"result: $resultOrErr")
    yield ()
}