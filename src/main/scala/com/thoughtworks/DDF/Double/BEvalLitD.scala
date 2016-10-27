package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.BEvalArrow
import com.thoughtworks.DDF.{BEval, Loss}

trait BEvalLitD extends BEvalDoubleInfo with BEvalArrow with LitD[Loss, BEval] {
  override def litD = dEval
}

object BEvalLitD {
  implicit def apply: BEvalLitD = new BEvalLitD { }
}