package com.thoughtworks.DDF

trait LossCase[X] extends TypeCase[LossCase, X]

object LossCase {
  type Aux[X, Y] = LossCase[X] {type ret = Y}
}