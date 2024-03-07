package io.koff.tf.effect_gathering.error

import cats.{Applicative, MonadError, MonadThrow}
import cats.data.{Chain, WriterT}
import cats.effect.SyncIO
import io.koff.tf.effect_gathering.ReliableWriterT
import cats.mtl.Tell

object ErrorExample {
  type Logs = Chain[Log]
  final case class Error(msg: String) extends AnyVal
  final case class Log(msg: String)   extends AnyVal

  def main(args: Array[String]): Unit = {
    type Eff1[A] = WriterT[SyncIO, Logs, A]

    def eff1: Eff1[Int] = for
      // this log record is lost
      _ <- Tell[Eff1, Logs].tell(Chain.one(Log("It is going to fail")))
      // we can see only this error
      _ <- MonadThrow[Eff1].raiseError(new RuntimeException("failure"))
    yield 1

    val result1: Either[Throwable, (Logs, Int)] = eff1.run.attempt.unsafeRunSync()
    println(s"result1: $result1") // result1: Left(java.lang.RuntimeException: failure)

    type Eff2[A] = ReliableWriterT[SyncIO, Throwable, Logs, A]

    def eff2Success: Eff2[Int] =
      for _ <- Tell[Eff2, Logs].tell(Chain.one(Log("Successful program")))
      yield 10

    def eff2RaiseError: Eff2[Int] = for
      _ <- Tell[Eff2, Logs].tell(Chain.one(Log("Faulty program")))
      _ <- MonadThrow[Eff2].raiseError(new RuntimeException("test error"))
    yield 10

    val (logs1, success) = eff2Success.run.unsafeRunSync()
    println(s"logs: $logs1; success: $success")
    // prints "logs: Chain(Log(Successful program)); success: Right(10)"

    val (logs2, err) = eff2RaiseError.run.unsafeRunSync()
    println(s"logs: $logs2; err: $err")
    // prints "logs: Chain(Log(Faulty program)); err: Left(test error)"
  }
}
