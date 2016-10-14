package com.thoughtworks.DDF

trait Loss[X] extends TypeCase[Loss, X] {
  final type loss = ret

  def m: CommutativeMonoid[loss]

  def convert: /*loss backprop ability*/ X => BEval[X]

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

trait BEvalCase[X] extends TypeCase[BEvalCase, X]

object BEvalCase {
  type Aux[X, Y] = BEvalCase[X] {type ret = Y}
}

trait BEval[X] {
  val loss: Loss[X]

  def eval: /*should not be used when defining instance of Eval*/ X

  val ec: BEvalCase[X]

  def eca: ec.ret
}