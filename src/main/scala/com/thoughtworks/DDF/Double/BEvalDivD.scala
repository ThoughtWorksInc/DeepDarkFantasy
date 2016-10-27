package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ArrowLoss
import com.thoughtworks.DDF.{BEval, Loss}

trait BEvalDivD extends BEvalLitD with DivD[Loss, BEval] {
  override def divD = arrowEval[scala.Double, scala.Double => scala.Double, DLoss, ArrowLoss[scala.Double, DLoss]](x =>
    (arrowEval[scala.Double, scala.Double, DLoss, DLoss](y =>
      (litD(deval(x) / deval(y)),
        l => DLoss(deval(x) * l.d * (-1 / (deval(y) * deval(y)))))),
      _.mapReduce(y => l => DLoss(l.d / deval(y)))(doubleInfo.m)))

}

object BEvalDivD {
  implicit def apply: BEvalDivD = new BEvalDivD { }
}