package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalArr extends Arr[LossInfo, BEval] with BEvalArrInfo {
  override def app[A, B] = f => x => aeval(f).forward(x).eb
}

object BEvalArr {
  implicit def apply: BEvalArr = new BEvalArr {}
}