package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ArrowLoss
import com.thoughtworks.DDF.{BEval, Loss}

trait BEvalPlusD extends BEvalLitD with PlusD[Loss, BEval] {
  override def plusD: BEval[scala.Double => scala.Double => scala.Double] =
    arrowEval[scala.Double, scala.Double => scala.Double, DLoss, ArrowLoss[scala.Double, DLoss]](l =>
      (arrowEval[scala.Double, scala.Double, DLoss, DLoss](
        r => (litD(deval(l) + deval(r)), rl => rl)),
        _.mapReduce(_ => l => l)(doubleInfo.m)))
}

object BEvalPlusD extends BEvalPlusD