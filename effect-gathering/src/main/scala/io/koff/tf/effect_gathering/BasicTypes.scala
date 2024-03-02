package io.koff.tf.effect_gathering

import cats.Show
import cats.data.{Chain, WriterT}
import cats.effect.IO
import cats.mtl.Tell
import cats.instances.string.*
import cats.instances.list.*
import cats.instances.int.*
import cats.instances.double.*

object BasicTypes {
  // These are our types that we are going to use in our examples
  type Input1  = String
  type Input2  = List[Char]
  type Output1 = Int
  type Output2 = Double

  given input1Show: Show[Input1]   = catsStdShowForString
  given input2Show: Show[Input2]   = catsStdShowForList[Char]
  given output1Show: Show[Output1] = catsStdShowForInt
  given output2Show: Show[Output2] = catsStdShowForDouble

  final type TellLogs[F[_], Elem] = Tell[F, Chain[Elem]]

  /** Adding this additional type alias to make our types a bit more readable. */
  final type TellTagLogs[F[_]] = TellLogs[F, Log]

  /** Our end-of-the-world effect */
  type Eff[T] = WriterT[IO, Chain[Log], T]
}
