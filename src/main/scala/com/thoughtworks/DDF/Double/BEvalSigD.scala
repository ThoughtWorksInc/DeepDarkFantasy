package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalSigD extends SigD[LossInfo, BEval] with BEvalLitD {
  override def sigD = arrowEval[scala.Double, scala.Double, DLoss, DLoss](x =>
    (litD(1 / (1 + Math.exp(- deval(x)))),
      l => DLoss(l.d * (1 / (1 + Math.exp(- deval(x)))) * (1 / (1 + Math.exp(- (1 - deval(x))))))))
}

object BEvalSigD {
  implicit def apply: BEvalSigD = new BEvalSigD { }
}