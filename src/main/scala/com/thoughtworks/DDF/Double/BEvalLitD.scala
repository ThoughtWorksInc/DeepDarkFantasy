package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.BEvalArr
import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalLitD extends BEvalDoubleInfo with BEvalArr with LitD[LossInfo, BEval] {
  override def litD = dEval
}

object BEvalLitD {
  implicit def apply: BEvalLitD = new BEvalLitD { }
}