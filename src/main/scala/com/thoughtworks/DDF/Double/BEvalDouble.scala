package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.{BEvalArrow, ArrowLoss}
import com.thoughtworks.DDF._
import scalaz.Leibniz._

trait BEvalDouble extends BEvalArrow with DoubleRepr[Loss, BEval] {
  case class DLoss(d: Double)

  object DoubleBEC extends BEvalCase[Double] {
    override type ret = Double
  }

  object DLC extends LossCase[Double] {
    override type ret = Unit
  }

  def dEval(d: Double) = new BEval[Double] {
    override val loss: Loss[Double] = doubleInfo

    override def eval: Double = d

    override val ec: BEvalCase.Aux[Double, Double] = DoubleBEC

    override def eca: ec.ret = d
  }

  def deval(d: BEval[Double]): Double = witness(d.ec.unique(DoubleBEC))(d.eca)

  override def litD: Double => BEval[Double] = dEval

  override def plusD: BEval[Double => Double => Double] =
    arrowEval[Double, Double => Double, DLoss, ArrowLoss[Double, DLoss]](l =>
      (arrowEval[Double, Double, DLoss, DLoss](
        r => (dEval(deval(l) + deval(r)), rl => rl)),
        _.mapReduce(_ => l => l)(doubleInfo.m)))

  override def multD: BEval[Double => Double => Double] =
    arrowEval[Double, Double => Double, DLoss, ArrowLoss[Double, DLoss]](l =>
      (arrowEval[Double, Double, DLoss, DLoss](
        r => (dEval(deval(l) * deval(r)), rl => DLoss(deval(l) * rl.d))),
        _.mapReduce(r => l => DLoss(deval(r) * l.d))(doubleInfo.m)))

  override implicit def doubleInfo: Loss.Aux[Double, DLoss] = new Loss[Double] {
    override def m: CommutativeMonoid[DLoss] = new CommutativeMonoid[DLoss] {
      override def zero: DLoss = DLoss(0)

      override def append(f1: DLoss, f2: => DLoss): DLoss = DLoss(f1.d + f2.d)
    }

    override def convert: Double => BEval[Double] = dEval

    override val lc: LossCase.Aux[Double, DLC.ret] = DLC

    override def lca: lc.ret = ()

    override type ret = DLoss

    override def update(x: Double)(rate: Double)(l: loss): Double = x - l.d * rate
  }

  override def expD = arrowEval[Double, Double, DLoss, DLoss](x =>
    (dEval(Math.exp(deval(x))), l => DLoss(l.d * Math.exp(deval(x)))))

  override def divD = arrowEval[Double, Double => Double, DLoss, ArrowLoss[Double, DLoss]](x =>
    (arrowEval[Double, Double, DLoss, DLoss](y =>
      (dEval(deval(x) / deval(y)),
        l => DLoss(deval(x) * l.d * (-1 / (deval(y) * deval(y)))))),
      _.mapReduce(y => l => DLoss(l.d / deval(y)))(doubleInfo.m)))
}

object BEvalDouble {
  implicit def apply = new BEvalDouble {}
}