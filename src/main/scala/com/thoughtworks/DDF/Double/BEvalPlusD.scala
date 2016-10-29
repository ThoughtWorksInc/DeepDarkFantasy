package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalPlusD extends BEvalLitD with PlusD[LossInfo, BEval] {
  override def plusD: BEval[scala.Double => scala.Double => scala.Double] =
    aEval[scala.Double, scala.Double => scala.Double](l =>
      (aEval[scala.Double, scala.Double](
        r => (litD(deval(l) + deval(r)), rl => rl)),
        x => aloss(x).mapReduce(_ => l => l)(doubleInfo.lm)))
}

object BEvalPlusD extends BEvalPlusD