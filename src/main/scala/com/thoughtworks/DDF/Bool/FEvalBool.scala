package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.FEvalArr
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}
import com.thoughtworks.DDF.{FEval, FEvalCase, FEvalMatch}

trait FEvalBool extends
  Bool[FEvalCase, FEval] with FEvalArr {
  override def ite[A](implicit ai: FEvalCase[A]) =
    new FEval[Boolean => A => A => A] {
      override val fec = aInfo(boolInfo, aInfo(ai, aInfo(ai, ai)))

      override def term[G: Gradient] = base.ite(ai.wgi[G])
    }

  override def litB: Boolean => FEval[Boolean] = b => new FEval[Boolean] {
    override val fec = boolInfo

    override def term[G: Gradient] = base.litB(b)
  }

  override implicit def boolInfo: FEvalCase.Aux[Boolean, Lambda[X => Boolean]] = new FEvalCase[Boolean] {
    override type WithGrad[_] = Boolean

    override val tm = new FEvalMatch[Boolean] {
      override type ret = Unit
    }

    override def tmr: tm.ret = ()

    override def wgi[G: Gradient]: LangInfoG[Boolean] = base.boolInfo
  }
}

object FEvalBool extends FEvalBool