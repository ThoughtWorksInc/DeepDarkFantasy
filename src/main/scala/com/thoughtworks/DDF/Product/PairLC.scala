package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.LossCase

case class PairLC[A, B]() extends LossCase[(A, B)] {
  override type ret = PairLCRet[A, B]
}
