package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.FEvalArr
import com.thoughtworks.DDF.Language.LangInfoG
import com.thoughtworks.DDF.{FEMMatch, FEval, FEvalCase}

trait FEvalBool[G] extends
  Bool[FEvalCase[G, ?], FEval[G, ?]] with FEvalArr[G] {
  override def ite[A](implicit ai: FEvalCase[G, A]) =
    new FEval[G, Boolean => A => A => A] {
      override val tm = aInfo(boolInfo, aInfo(ai, aInfo(ai, ai)))

      override val deriv = base.ite(ai.lr)
    }

  override def litB: Boolean => FEval[G, Boolean] = b => new FEval[G, Boolean] {
    override val tm = boolInfo

    override val deriv = base.litB(b)
  }

  override implicit def boolInfo: FEvalCase.Aux[G, Boolean, Boolean] = new FEvalCase[G, Boolean] {
    override type ret = Boolean

    override val tm: FEMMatch.Aux[G, Boolean, Unit] = new FEMMatch[G, Boolean] {
      override type ret = Unit
    }

    override def tmr: tm.ret = ()

    override def lr: LangInfoG[Boolean] = base.boolInfo
  }
}

object FEvalBool {
  implicit def apply[G]: FEvalBool[G] = new FEvalBool[G] { }
}