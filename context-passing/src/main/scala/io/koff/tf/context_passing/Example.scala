package io.koff.tf.context_passing

import cats.mtl.Ask

trait Example {
  type F[A]
  type Context
  type Input
  type Output

  type HasCtx[F[_]] = Ask[F, Context]

  trait Layer1:
    def operation1(in: Input): F[Output]

  trait Layer2:
    def operation2(in: Input): F[Output]

  trait Layer3:
    def operation3(in: Input): F[Output]

  trait Layer4:
    def operation4(in: Input): F[Output]
}
