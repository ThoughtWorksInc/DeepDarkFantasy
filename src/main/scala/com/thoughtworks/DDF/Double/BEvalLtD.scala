package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ArrowLoss
import com.thoughtworks.DDF.Bool.BEvalBool
import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalLtD extends LtD[LossInfo, BEval] with BEvalLitD with BEvalBool {
  override def ltD = aEval[scala.Double, scala.Double => Boolean](x =>
    (aEval[scala.Double, Boolean](y =>
      (litB(deval(x) < deval(y)),
        _ => doubleInfo.lm.zero)),
      _ => doubleInfo.lm.zero))
}
