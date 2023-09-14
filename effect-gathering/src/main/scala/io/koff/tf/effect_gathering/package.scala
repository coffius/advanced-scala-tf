package io.koff.tf

package object effect_gathering {
  // Effect gathering
  // Idea: It is possible to gather certain types of effects as a collection of data structures.
  // Motivation: This approach makes possible to analyse these effects later.
  // Examples:
  // 1.  Short description of the `Tell[F, ..]` type class and how it helps with effect gathering on an example of internal logging
  // 2.  Logging as a capability - abstraction of Tell[F, ..] + similar example of internal logging
  // 3.  External logging - universal logging of any service interface
  // 3.a Interface as an ADT
  // 3.b Two implementations of the interface - with logic and with data structure + conversion between them using natural transformations
  // 4.  Errors, fail fast and Reliable WriterT

}
