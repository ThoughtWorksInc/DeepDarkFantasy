package com.thoughtworks.DDF

import scalaz.Monoid

trait Loss[X] extends TypeCase[Loss, X] {
  final type loss = ret

  def m: CommutativeMonoid[loss]

  def convert: /*loss backprop ability*/ X => Eval[X]

  val lc: LossCase[X]

  def lca: lc.ret

  def update(x: X)(rate: Double)(l: loss): X
}

object Loss {
  type Aux[X, XL] = Loss[X] {type ret = XL}
}

trait LossCase[X] extends TypeCase[LossCase, X]

object LossCase {
  type Aux[X, Y] = LossCase[X] {type ret = Y}
}

trait EvalCase[X] extends TypeCase[EvalCase, X]

object EvalCase {
  type Aux[X, Y] = EvalCase[X] {type ret = Y}
}

trait Eval[X] {
  val loss: Loss[X]

  def eval: /*should not be used when defining instance of Eval*/ X

  val ec: EvalCase[X]

  def eca: ec.ret
}