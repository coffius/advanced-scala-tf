package io.koff.derivation.lens.compile

import scala.compiletime.{erasedValue, error, summonInline}

object Compile {
  private inline def getFromTuple[T <: Tuple, A](t: T): A = inline t match
    case tup: (head *: tail) =>
      inline erasedValue[head] match
        case _: A =>
          val ev = summonInline[head =:= A]
          ev(tup.head)
        case _ => getFromTuple[tail, A](tup.tail)
    case _ => error("value of A not found in Tuple")
  def main(args: Array[String]): Unit = {
    inline def tuple1: (Long, String, Double) = (1L, "string", 0.0d)
    val result                                = getFromTuple(tuple1)
    println(result)
  }

  inline def testType[A, B](a: A): B =
    inline erasedValue[A] match
      case _: B =>
        val ev = summonInline[A =:= B]
        ev(a)
      case _ => error("A !:= B")
}
