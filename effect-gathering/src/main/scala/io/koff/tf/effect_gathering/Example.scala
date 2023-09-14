package io.koff.tf.effect_gathering

import cats.data.Chain
import cats.mtl.Tell
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.show.*
import cats.{Applicative, Monad, Monoid, Show}
import cats.mtl.syntax.tell.*
import TellExtension.tellOne

trait Example {
  type Input1
  type Input2
  type Output1
  type Output2

  given input1Show: Show[Input1]
  given input2Show: Show[Input2]
  given output1Show: Show[Output1]
  given output2Show: Show[Output2]

  final type TellLogs[F[_], LogElem] = Tell[F, Chain[LogElem]]

  /** Example of half structured log - some of cases have additional info */
  enum AppLog:
    case Info(input: String, output: String)
    case Warn(value: String)
    case Error(value: String)

  final type TellAppLogs[F[_]] = TellLogs[F, AppLog]

  enum GenericLog:
    case Success(opName: String, input: String, output: String)
    case Error(opName: String, input: String, error: String)
  trait HighLvlService[F[_]]:
    def operation1(in: Input1): F[Output1]
    def operation2(in: Input2): F[Output2]

  private final class RealHighLvlServiceImpl[F[_]: Monad](next: MiddleLvlService[F])(using
      T: TellAppLogs[F]
  ) extends HighLvlService[F]:
    override def operation1(in: Input1): F[Output1] = for
      out1 <- next.operation1A(in)
      _    <- tellOne(AppLog.Info(in.show, out1.show))
      out2 <- next.operation1B(in)
      _    <- tellOne(AppLog.Info(in.show, out2.show))
    yield out1
    override def operation2(in: Input2): F[Output2] = for
      out1 <- next.operation2A(in)
      _    <- tellOne(AppLog.Info(in.show, out1.show))
      out2 <- next.operation2B(in)
      _    <- tellOne(AppLog.Info(in.show, out2.show))
    yield out1

  trait MiddleLvlService[F[_]]:
    def operation1A(in: Input1): F[Output1]
    def operation1B(in: Input1): F[Output1]
    def operation2A(in: Input2): F[Output2]
    def operation2B(in: Input2): F[Output2]

  enum MiddleLvlOps[Out]:
    case Operation1A(in: Input1) extends MiddleLvlOps[Output1]
    case Operation1B(in: Input1) extends MiddleLvlOps[Output1]
    case Operation2A(in: Input2) extends MiddleLvlOps[Output2]
    case Operation2B(in: Input2) extends MiddleLvlOps[Output2]

  private final class OpsBasedMiddleLvlServiceImpl extends MiddleLvlService[MiddleLvlOps]:
    override def operation1A(in: Input1): MiddleLvlOps[Output1] = MiddleLvlOps.Operation1A(in)
    override def operation1B(in: Input1): MiddleLvlOps[Output1] = MiddleLvlOps.Operation1B(in)
    override def operation2A(in: Input2): MiddleLvlOps[Output2] = MiddleLvlOps.Operation2A(in)
    override def operation2B(in: Input2): MiddleLvlOps[Output2] = MiddleLvlOps.Operation2B(in)

  private final class RealMiddleLvlServiceImpl[F[_]] extends MiddleLvlService[F]:
    override def operation1A(in: Input1): F[Output1] = ???
    override def operation1B(in: Input1): F[Output1] = ???
    override def operation2A(in: Input2): F[Output2] = ???
    override def operation2B(in: Input2): F[Output2] = ???

  trait LowLvlService[F[_]]:
    def operation3(in: Input1): F[Output1]

  trait Layer4[F[_]]:
    def operation4(in: Input1): F[Output1]
}
