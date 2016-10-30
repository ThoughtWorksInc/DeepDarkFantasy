package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.BEvalArrow
import com.thoughtworks.DDF.Combinators.{BEvalComb, Comb}
import com.thoughtworks.DDF.{BEval, BEvalMatch, CommutativeMonoid, CommutativeMonoidUnit, Loss, LossMatch, LossInfo}

trait BEvalBool extends Bool[LossInfo, BEval] with BEvalArrow {
  object BoolBEC extends BEvalMatch[Boolean] {
    override type ret = Boolean
  }

  override def litB: Boolean => BEval[Boolean] = b => new BEval[Boolean] {
    override val tmr: tm.ret = b

    override def eval: Boolean = b

    override val tm: BEvalMatch.Aux[Boolean, Boolean] = BoolBEC

    override val loss: LossInfo[Boolean] = boolInfo
  }

  def beval: BEval[Boolean] => Boolean = _.get(BoolBEC)

  private def comb: Comb[LossInfo, BEval] = BEvalComb.apply

  val bLoss: Loss[Boolean] = new Loss[Boolean] {
    override val tm: LossInfo.Aux[Boolean, Unit] = boolInfo

    override val tmr: tm.loss = ()
  }

  override def ite[A](implicit ai: LossInfo[A]): BEval[Boolean => A => A => A] =
    aEval[Boolean, A => A => A](b =>
      (if(beval(b))
        comb.K[A, A](ai, ai) else
        app(comb.C[A, A, A](ai, ai, ai))(comb.K[A, A](ai, ai)), _ => bLoss))(boolInfo, aInfo(ai, aInfo(ai, ai)))

  override implicit def boolInfo: LossInfo.Aux[Boolean, Unit] = new LossInfo[Boolean] {
    override def convert = litB

    override def m: CommutativeMonoid[Unit] = CommutativeMonoidUnit.apply

    override val tmr: tm.ret = ()

    override val tm: LossMatch.Aux[Boolean, Unit] = new LossMatch[Boolean] {
      override type ret = Unit
    }

    override type ret = Unit

    override def update(x: Boolean)(rate: Double)(l: loss): Boolean = x
  }
}

object BEvalBool {
  implicit def apply = new BEvalBool {}
}
