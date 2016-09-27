package com.thoughtworks.DDF

trait EvalCase[X] extends TypeCase[EvalCase, X]

object EvalCase {
  type Aux[X, Y] = EvalCase[X] {type ret = Y}
}