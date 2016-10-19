package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.BEvalArrow
import com.thoughtworks.DDF.{BEval, BEvalCase, CommutativeMonoid, Loss, LossCase}
import scalaz.Leibniz._

trait BEvalDoubleInfo extends BEvalArrow with DoubleInfo[Loss, BEval] {
  case class DLoss(d: Double)

  object DLC extends LossCase[Double] {
    override type ret = Unit
  }

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

  object DoubleBEC extends BEvalCase[Double] {
    override type ret = Double
  }

  def dEval(d: Double) = new BEval[Double] {
    override val loss: Loss[Double] = doubleInfo

    override def eval: Double = d

    override val ec: BEvalCase.Aux[Double, Double] = DoubleBEC

    override def eca: ec.ret = d
  }

  def deval(d: BEval[Double]): Double = witness(d.ec.unique(DoubleBEC))(d.eca)
}

object BEvalDoubleInfo {
  implicit def apply = new BEvalDoubleInfo {}
}