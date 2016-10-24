package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.BEvalArrowInfo
import com.thoughtworks.DDF.{BEval, BEvalCase, CommutativeMonoid, Loss, LossCase}

import scalaz.Leibniz._

trait BEvalDoubleInfo extends BEvalArrowInfo with DoubleInfo[Loss, BEval] {
  case class DLoss(d: scala.Double)

  object DLC extends LossCase[scala.Double] {
    override type ret = Unit
  }

  override implicit def doubleInfo: Loss.Aux[scala.Double, DLoss] = new Loss[scala.Double] {
    override def m: CommutativeMonoid[DLoss] = new CommutativeMonoid[DLoss] {
      override def zero: DLoss = DLoss(0)

      override def append(f1: DLoss, f2: => DLoss): DLoss = DLoss(f1.d + f2.d)
    }

    override def convert: scala.Double => BEval[scala.Double] = dEval

    override val lc: LossCase.Aux[scala.Double, DLC.ret] = DLC

    override def lca: lc.ret = ()

    override type ret = DLoss

    override def update(x: scala.Double)(rate: scala.Double)(l: loss): scala.Double = x - l.d * rate
  }

  object DoubleBEC extends BEvalCase[scala.Double] {
    override type ret = scala.Double
  }

  def dEval(d: scala.Double) = new BEval[scala.Double] {
    override val loss: Loss[scala.Double] = doubleInfo

    override def eval: scala.Double = d

    override val ec: BEvalCase.Aux[scala.Double, scala.Double] = DoubleBEC

    override def eca: ec.ret = d
  }

  def deval(d: BEval[scala.Double]): scala.Double = witness(d.ec.unique(DoubleBEC))(d.eca)
}

object BEvalDoubleInfo {
  implicit def apply = new BEvalDoubleInfo {}
}