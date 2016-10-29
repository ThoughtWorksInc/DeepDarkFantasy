package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ArrowLoss
import com.thoughtworks.DDF.{BEval, LossInfo}

trait BEvalMultD extends BEvalLitD with MultD[LossInfo, BEval] {
  override def multD: BEval[scala.Double => scala.Double => scala.Double] =
    aEval[scala.Double, scala.Double => scala.Double](l =>
      (aEval[scala.Double, scala.Double](
        r => (litD(deval(l) * deval(r)), rl => lossD(deval(l) * dloss(rl)))),
        x => aloss(x).mapReduce(r => l => lossD(deval(r) * dloss(l)))(doubleInfo.lm)))
}

object BEvalMultD {
  implicit def apply: BEvalMultD = new BEvalMultD { }
}