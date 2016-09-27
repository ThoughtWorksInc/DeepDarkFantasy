package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.LossCase

case class SumLC[A, B]() extends LossCase[Either[A, B]] {
  override type ret = SumLCRet[A, B]
}
