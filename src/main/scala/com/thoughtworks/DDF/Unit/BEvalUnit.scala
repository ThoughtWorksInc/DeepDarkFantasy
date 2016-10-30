package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.InfoBase.BEvalInfoBase
import com.thoughtworks.DDF.{BEval, BEvalMatch, CommutativeMonoid, CommutativeMonoidUnit, LossMatch, LossInfo}

trait BEvalUnit extends Unit[LossInfo, BEval] with BEvalInfoBase {
  override implicit def unitInfo: LossInfo.Aux[scala.Unit, scala.Unit] = new LossInfo[scala.Unit] {
    override type ret = scala.Unit

    override def m: CommutativeMonoid[scala.Unit] = CommutativeMonoidUnit.apply

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

    override implicit val loss: LossInfo[scala.Unit] = unitInfo
  }

}

object BEvalUnit {
  implicit def apply = new BEvalUnit {}
}