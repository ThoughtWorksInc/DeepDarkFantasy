package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.Arrow.BEvalArr
import com.thoughtworks.DDF.{BEval, CommutativeMonoid, CommutativeMonoidUnit, LossInfo, LossMatch}

trait BEvalBot extends Bot[LossInfo, BEval] with BEvalArr {
  override def exfalso[A](implicit ai: LossInfo[A]): BEval[Nothing => A] =
    aEval[Nothing, A](_.eval)

  override implicit def botInfo: LossInfo[Nothing] = new LossInfo[Nothing] {
    override def update(x: Nothing)(rate: Double)(l: Unit): Nothing = x

    override def convert: Nothing => BEval[Nothing] = x => x

    override def m: CommutativeMonoid[Unit] = CommutativeMonoidUnit

    override type ret = Unit

    override def tmr = ()

    override val tm = new LossMatch[Nothing] {
      override type ret = Unit
    }
  }
}

object BEvalBot extends BEvalBot