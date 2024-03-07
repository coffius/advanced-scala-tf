package io.koff.tf.effect_gathering.capability

import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.show.*
import io.koff.tf.effect_gathering.BasicTypes

/** In this example we use `LogCapability[F]` instead of `T: TellTagLogs[F]` */
trait LogCapabilityExample extends BasicTypes {
  trait Service[F[_]]:
    def operation1(in: Input1): F[Output1]
    def operation2(in: Input2): F[Output2]

  protected final class ServiceImpl[F[_]: Monad: LogCapability](
      lowLvlOp1: Input1 => F[Output1],
      lowLvlOp2: Output1 => F[Output2],
      lowLvlOp3: Input2 => F[Output1]
  ) extends Service[F]:
    private val Log: LogCapability[F] = LogCapability[F]
    override def operation1(in: Input1): F[Output1] = for
      out1 <- lowLvlOp1(in)
      _    <- Log.info("operation1", in.show, out1.show)
      out2 <- lowLvlOp2(out1)
      _    <- Log.info("operation1", in.show, out2.show)
    yield out1

    override def operation2(in: Input2): F[Output2] = for
      out1 <- lowLvlOp3(in)
      _    <- Log.info("operation2", in.show, out1.show)
      out2 <- lowLvlOp2(out1)
      _    <- Log.info("operation2", in.show, out2.show)
    yield out2
}
