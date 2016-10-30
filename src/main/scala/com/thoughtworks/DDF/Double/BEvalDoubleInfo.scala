package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.BEvalArrowInfo
import com.thoughtworks.DDF.{BEval, BEvalMatch, CommutativeMonoid, Loss, LossMatch, LossInfo}

trait BEvalDoubleInfo extends BEvalArrowInfo with DoubleInfo[LossInfo, BEval] {
  def lossD: scala.Double => Loss[scala.Double] = y => new Loss[scala.Double] {
    override val tmr: tm.loss = y

    override val tm: LossInfo.Aux[scala.Double, scala.Double] = doubleInfo
  }

  def dloss: Loss[scala.Double] => scala.Double = _.get(doubleInfo)

  object DLC extends LossMatch[scala.Double] {
    override type ret = Unit
  }

  override implicit def doubleInfo: LossInfo.Aux[scala.Double, scala.Double] = new LossInfo[scala.Double] {
    override def m: CommutativeMonoid[scala.Double] = new CommutativeMonoid[scala.Double] {
      override def zero: scala.Double = 0

      override def append(f1: scala.Double, f2: => scala.Double): scala.Double = f1 + f2
    }

    override def convert: scala.Double => BEval[scala.Double] = dEval

    override val tm: LossMatch.Aux[scala.Double, DLC.ret] = DLC

    override val tmr: tm.ret = ()

    override type ret = scala.Double

    override def update(x: scala.Double)(rate: scala.Double)(l: loss): scala.Double = x - l * rate
  }

  object DoubleBEC extends BEvalMatch[scala.Double] {
    override type ret = scala.Double
  }

  def dEval(d: scala.Double) = new BEval[scala.Double] {
    override val loss: LossInfo[scala.Double] = doubleInfo

    override def eval: scala.Double = d

    override val tm: BEvalMatch.Aux[scala.Double, scala.Double] = DoubleBEC

    override val tmr: tm.ret = d
  }

  def deval(d: BEval[scala.Double]): scala.Double = d.get(DoubleBEC)
}

object BEvalDoubleInfo {
  implicit def apply = new BEvalDoubleInfo {}
}