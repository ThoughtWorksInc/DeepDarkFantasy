package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.InfoBase.BEvalInfoBase
import com.thoughtworks.DDF.{BEval, BEvalMatch, CommutativeMonoid, CommutativeMonoidUnit, LossMatch, LossInfo}

trait BEvalTop extends Top[LossInfo, BEval] with BEvalInfoBase {
  override implicit def topInfo: LossInfo.Aux[scala.Unit, scala.Unit] = new LossInfo[scala.Unit] {
    override type ret = scala.Unit

    override def m: CommutativeMonoid[scala.Unit] = CommutativeMonoidUnit

    override def convert: scala.Unit => BEval[scala.Unit] = _ => mkUnit

    override val tm: LossMatch.Aux[scala.Unit, scala.Unit] = new LossMatch[scala.Unit] {
      override type ret = scala.Unit
    }

    override val tmr: tm.ret = ()

    override def update(x: scala.Unit)(rate: Double)(l: loss): scala.Unit = ()
  }

  override def mkUnit: BEval[scala.Unit] = new BEval[scala.Unit] {
    override val tmr: tm.ret = ()

    override def eval: scala.Unit = ()

    override val tm: BEvalMatch.Aux[scala.Unit, scala.Unit] = new BEvalMatch[scala.Unit] {
      override type ret = scala.Unit
    }

    override implicit val loss: LossInfo[scala.Unit] = topInfo
  }

}

object BEvalTop {
  implicit def apply = new BEvalTop {}
}