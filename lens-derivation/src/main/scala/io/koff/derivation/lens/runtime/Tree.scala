package io.koff.derivation.lens.runtime

import cats.data.NonEmptyList
enum FieldType {
  def name: String
  case Simple(name: String)
  case Complex(name: String, fields: NonEmptyList[Field])
}
final case class Field(name: String, fieldType: FieldType)

object Algorithm {
  def findPath(start: FieldType, toFind: FieldType): Option[NonEmptyList[FieldType]] = {
    start match
      case fType @ FieldType.Simple(name) =>
        if (toFind.name == name) {
          Some(NonEmptyList.one(fType))
        } else {
          None
        }
      case fType @ FieldType.Complex(name, fields) =>
        if (toFind.name == name) {
          Some(NonEmptyList.one(fType))
        } else {
          val zero: Option[NonEmptyList[FieldType]] = None
          fields.foldLeft(zero) { (acc, field) =>
            acc match
              case defined: Some[a] => defined
              case None =>
                val maybeFound = findPath(field.fieldType, toFind)
                maybeFound.map(prev => prev.prepend(start))
          }
        }
  }

  def main(args: Array[String]): Unit = {
    val accountType   = FieldType.Simple("Account")
    val firstNameType = FieldType.Simple("FirstName")
    val lastNameType  = FieldType.Simple("LastName")
    val userNameType = FieldType.Complex(
      "UserName",
      NonEmptyList.of(Field("firstName", firstNameType), Field("lastName", lastNameType))
    )
    val cityType   = FieldType.Simple("City")
    val streetType = FieldType.Simple("Street")
    val addressType = FieldType.Complex(
      "Address",
      NonEmptyList.of(Field("city", cityType), Field("street", streetType))
    )
    val userType = FieldType.Complex(
      "User",
      NonEmptyList.of(Field("userName", userNameType), Field("address", addressType))
    )

    val found1 = findPath(userType, firstNameType).map(_.map(_.name))
    println(found1)
    val found2 = findPath(userType, addressType).map(_.map(_.name))
    println(found2)
    val notFound = findPath(userType, accountType).map(_.map(_.name))
    println(notFound)
  }
}
