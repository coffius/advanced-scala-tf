package io.koff.derivation.lens

final case class City(value: String)
final case class Street(value: String)
final case class Address(city: City, street: Street)
final case class FirstName(value: String)
final case class SecondName(value: String)
final case class UserName(firstName: FirstName, secondName: SecondName)
final case class User(userName: UserName, address: Address)
