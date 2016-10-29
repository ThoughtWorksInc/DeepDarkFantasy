package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalDivD extends BEvalLitD with DivD[LossInfo, BEval] {
  override def divD = aEval[scala.Double, scala.Double => scala.Double](x =>
    (aEval[scala.Double, scala.Double](y =>
      (litD(deval(x) / deval(y)),
        l => lossD(deval(x) * dloss(l) * (-1 / (deval(y) * deval(y)))))),
      x => aloss(x).mapReduce(y => l => lossD(dloss(l) / deval(y)))(doubleInfo.lm)))

}

object BEvalDivD {
  implicit def apply: BEvalDivD = new BEvalDivD { }
}