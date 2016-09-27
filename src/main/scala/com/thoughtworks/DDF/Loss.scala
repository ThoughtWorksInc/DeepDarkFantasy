package com.thoughtworks.DDF

import scalaz.Monoid

trait Loss[X] extends TypeCase[Loss, X] {
  final type loss = ret

  def m: Monoid[loss]

  def conv: /*loss backprop ability*/ X => Eval[X]

  val lc: LossCase[X]

  def lca: lc.ret
}

object Loss {
  type Aux[X, XL] = Loss[X] {type ret = XL}
}