package io.koff.tf.effect_gathering

/** Example of half structured log - its cases have additional info. We are going to use it as a
  * data structure for gathering info needed to be logged.
  */
enum Log:
  case Info(tag: String, input: String, output: String)
  case Error(tag: String, input: String, error: Throwable)
