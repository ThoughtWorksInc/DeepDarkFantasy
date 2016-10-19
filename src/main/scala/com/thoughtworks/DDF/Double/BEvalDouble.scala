package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.{ArrowLoss, BEvalArrow}
import com.thoughtworks.DDF.{BEval, Loss}

trait BEvalDouble extends BEvalDoubleInfo with BEvalArrow with DoubleRepr[Loss, BEval] {
  override def litD = dEval

  override def plusD: BEval[Double => Double => Double] =
    arrowEval[Double, Double => Double, DLoss, ArrowLoss[Double, DLoss]](l =>
      (arrowEval[Double, Double, DLoss, DLoss](
        r => (litD(deval(l) + deval(r)), rl => rl)),
        _.mapReduce(_ => l => l)(doubleInfo.m)))

  override def multD: BEval[Double => Double => Double] =
    arrowEval[Double, Double => Double, DLoss, ArrowLoss[Double, DLoss]](l =>
      (arrowEval[Double, Double, DLoss, DLoss](
        r => (litD(deval(l) * deval(r)), rl => DLoss(deval(l) * rl.d))),
        _.mapReduce(r => l => DLoss(deval(r) * l.d))(doubleInfo.m)))

  override def expD = arrowEval[Double, Double, DLoss, DLoss](x =>
    (litD(Math.exp(deval(x))), l => DLoss(l.d * Math.exp(deval(x)))))

  override def divD = arrowEval[Double, Double => Double, DLoss, ArrowLoss[Double, DLoss]](x =>
    (arrowEval[Double, Double, DLoss, DLoss](y =>
      (litD(deval(x) / deval(y)),
        l => DLoss(deval(x) * l.d * (-1 / (deval(y) * deval(y)))))),
      _.mapReduce(y => l => DLoss(l.d / deval(y)))(doubleInfo.m)))

  override def sigD = arrowEval[Double, Double, DLoss, DLoss](x =>
    (litD(1 / (1 + Math.exp(- deval(x)))),
      l => DLoss(l.d * (1 / (1 + Math.exp(- deval(x)))) * (1 / (1 + Math.exp(- (1 - deval(x))))))))
}

object BEvalDouble {
  implicit def apply = new BEvalDouble {}
}