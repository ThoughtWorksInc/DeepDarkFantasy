package com.thoughtworks.DDF.Arr

import com.thoughtworks.DDF.Eval.LossCase

case class ArrLC[A, B]() extends LossCase[A => B] {
  override type ret = ArrLCRet[A, B]
}
