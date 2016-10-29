package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalDouble extends
  Double[LossInfo, BEval] with
  BEvalPlusD with
  BEvalMultD with
  BEvalDivD with
  BEvalExpD with
  BEvalSigD with
  BEvalLtD

object BEvalDouble {
  implicit def apply = new BEvalDouble {}
}