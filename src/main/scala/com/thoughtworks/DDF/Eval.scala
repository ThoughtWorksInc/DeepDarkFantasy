package com.thoughtworks.DDF

trait Eval[X] {
  val loss: Loss[X]

  def eval: /*should not be used when defining instance of Eval*/ X

  val ec: EvalCase[X]

  def eca: ec.ret
}