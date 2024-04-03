package io.koff.derivation.lens.compile

import io.koff.derivation.lens.*

import scala.compiletime.*
import scala.deriving.*
import scala.deriving.Mirror.Of
import scala.quoted.{Expr, Quotes, Type}
import Macros.*

object Compile2 {
  private inline def cantDeriveCompileError[S, A] =
    error("Can't derive DGetter[" + showType[S] + ", " + showType[A] + "]")
  inline transparent given deriveGetter2[S, A](using
      mir: Mirror.Of[S]
  ): DGetter[S, A] = inline mir match
    case _: Mirror.SumOf[S] => error("Sum types does not supported yet")
    case m: Mirror.ProductOf[S] =>
      inline makeProductGetter2[S, A](using m) match
        case Some(a) => a
        case None    => cantDeriveCompileError[S, A]

  private inline transparent def makeGetter[ToCheck, A]: Option[DGetter[ToCheck, A]] =
    inline erasedValue[ToCheck] match
      case _: A =>
        Some {
          new DGetter[ToCheck, A]:
            override def get(toCheck: ToCheck): A = toCheck.asInstanceOf[A]
        }
      case _: Product =>
        makeProductGetter2[ToCheck, A](using summonInline[Mirror.ProductOf[ToCheck]])
      case _ => None

  private inline transparent def headGetter[S, Head](using
      m: Mirror.ProductOf[S]
  ): DGetter[S, Head] =
    inline erasedValue[m.MirroredElemTypes] match
      case _: (head *: tail) =>
        inline erasedValue[head] match
          case _: Head =>
            new DGetter[S, Head]:
              override def get(s: S): Head =
                val asTuple = Tuple.fromProduct(s.asInstanceOf[Product])
                asTuple.productElement(0).asInstanceOf[Head]

      case _ => error("head is not head")

  private inline transparent def tailGetter[S, Tail](using
      m: Mirror.ProductOf[S]
  ): DGetter[S, Tail] =
    inline erasedValue[m.MirroredElemTypes] match
      case _: (head *: tail) =>
        inline erasedValue[tail] match
          case _: Tail =>
            new DGetter[S, Tail]:
              override def get(s: S): Tail =
                val asTuple = Tuple.fromProduct(s.asInstanceOf[Product]).asInstanceOf[NonEmptyTuple]
                asTuple.tail.asInstanceOf[Tail]

          case _ => error("tail is not Tail!")
      case _ => error("S is an empty tuple")

  private inline transparent def makeProductGetter2[S, A](using
      mir: Mirror.ProductOf[S]
  ): Option[DGetter[S, A]] =
    inline erasedValue[mir.MirroredElemTypes] match
      case _: (head *: tail) =>
        inline makeGetter[head, A] match
          case Some(headA) => Some(headGetter[S, head].andThen(headA))
          case None =>
            inline makeGetter[tail, A] match
              case Some(tailA) => Some(tailGetter[S, tail].andThen(tailA))
              case None        => None
      case _: EmptyTuple => None

  def main(args: Array[String]): Unit = {
    val user = User(
      UserName(FirstName("Test"), SecondName("User")),
      Address(City("Riga"), Street("Brivibas"))
    )

    printField[User, FirstName](user)
    printField[User, SecondName](user)
    printField[User, UserName](user)
    printField[User, City](user)
    printField[User, Street](user)
    printField[User, Address](user)
//  type CustomUnknownType
//  printField[User, CustomUnknownType](user) // Can't derive DGetter[io.koff.derivation.lens.User, CustomUnknownType]
  }

  private def printField[S, A](s: S)(using dg: DGetter[S, A]): Unit =
    val value = dg.get(s)
    println(s"value: $value")
}
