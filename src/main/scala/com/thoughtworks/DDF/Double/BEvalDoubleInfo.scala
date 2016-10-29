package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.BEvalArrowInfo
import com.thoughtworks.DDF.{BEval, BEvalCase, CommutativeMonoid, Loss, LossCase, LossInfo}

import scalaz.Leibniz._

trait BEvalDoubleInfo extends BEvalArrowInfo with DoubleInfo[LossInfo, BEval] {
  def lossD: scala.Double => Loss[scala.Double] = y => new Loss[scala.Double] {
    override val x: li.loss = y

    override val li: LossInfo.Aux[scala.Double, scala.Double] = doubleInfo
  }

  def dloss: Loss[scala.Double] => scala.Double = x => witness(x.li.unique(doubleInfo))(x.x)

  object DLC extends LossCase[scala.Double] {
    override type ret = Unit
  }

  override implicit def doubleInfo: LossInfo.Aux[scala.Double, scala.Double] = new LossInfo[scala.Double] {
    override def m: CommutativeMonoid[scala.Double] = new CommutativeMonoid[scala.Double] {
      override def zero: scala.Double = 0

      override def append(f1: scala.Double, f2: => scala.Double): scala.Double = f1 + f2
    }

    override def convert: scala.Double => BEval[scala.Double] = dEval

    override val lc: LossCase.Aux[scala.Double, DLC.ret] = DLC

    override def lca: lc.ret = ()

    override type ret = scala.Double

    override def update(x: scala.Double)(rate: scala.Double)(l: loss): scala.Double = x - l * rate
  }

  object DoubleBEC extends BEvalCase[scala.Double] {
    override type ret = scala.Double
  }

  def dEval(d: scala.Double) = new BEval[scala.Double] {
    override val loss: LossInfo[scala.Double] = doubleInfo

    override def eval: scala.Double = d

    override val ec: BEvalCase.Aux[scala.Double, scala.Double] = DoubleBEC

    override def eca: ec.ret = d
  }

  def deval(d: BEval[scala.Double]): scala.Double = witness(d.ec.unique(DoubleBEC))(d.eca)
}

object BEvalDoubleInfo {
  implicit def apply = new BEvalDoubleInfo {}
}