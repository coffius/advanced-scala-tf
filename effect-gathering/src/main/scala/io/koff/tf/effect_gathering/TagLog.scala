package io.koff.tf.effect_gathering

enum TagLog:
  case Info(tag: String, input: String, output: String)
  case Error(tag: String, input: String, error: Throwable)
