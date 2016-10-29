package com.thoughtworks.DDF

trait LossInfo[X] extends TypeCase[LossInfo, X] {
  final type loss = ret

  def m: CommutativeMonoid[loss]

  def convert: /*lost backprop ability*/ X => BEval[X]

  val lc: LossCase[X]

  def lca: lc.ret

  def update(x: X)(rate: Double)(l: loss): X
}

object LossInfo {
  type Aux[X, XL] = LossInfo[X] {type ret = XL}
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
  val loss: LossInfo[X]

  def eval: /*should not be used when defining instance of Eval*/ X

  val ec: BEvalCase[X]

  def eca: ec.ret
}