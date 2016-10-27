package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ArrowLoss
import com.thoughtworks.DDF.{BEval, Loss}

trait BEvalMultD extends BEvalLitD with MultD[Loss, BEval] {
  override def multD: BEval[scala.Double => scala.Double => scala.Double] =
    arrowEval[scala.Double, scala.Double => scala.Double, DLoss, ArrowLoss[scala.Double, DLoss]](l =>
      (arrowEval[scala.Double, scala.Double, DLoss, DLoss](
        r => (litD(deval(l) * deval(r)), rl => DLoss(deval(l) * rl.d))),
        _.mapReduce(r => l => DLoss(deval(r) * l.d))(doubleInfo.m)))
}

object BEvalMultD {
  implicit def apply: BEvalMultD = new BEvalMultD { }
}