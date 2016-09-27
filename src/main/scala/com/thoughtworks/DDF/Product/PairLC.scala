package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Eval.LossCase

case class PairLC[A, B]() extends LossCase[(A, B)] {
  override type ret = PairLCRet[A, B]
}
