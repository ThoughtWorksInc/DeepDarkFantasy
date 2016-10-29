package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ArrowLoss
import com.thoughtworks.DDF.Bool.BEvalBool
import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalLtD extends LtD[LossInfo, BEval] with BEvalLitD with BEvalBool {
  override def ltD = arrowEval[scala.Double, scala.Double => Boolean, DLoss, ArrowLoss[scala.Double, Unit]](x =>
    (arrowEval[scala.Double, Boolean, DLoss, Unit](y =>
      (litB(deval(x) < deval(y)),
        _ => doubleInfo.m.zero)),
      _ => doubleInfo.m.zero))
}
