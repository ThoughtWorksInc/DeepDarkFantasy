package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.{Eval, EvalCase}

case class PairEC[A, B]() extends EvalCase[(A, B)] {
  override type ret = (Eval[A], Eval[B])
}
