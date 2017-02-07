package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.ADEvalArr
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.Language.LangInfoG
import com.thoughtworks.DDF.{ADEval, ADEvalCase, ADEvalMatch}
import com.thoughtworks.DDF.RecursiveInfoMatch._

trait ADEvalBool extends
  Bool[ADEvalCase, ADEval] with ADEvalArr {
  override def ite[A](implicit ai: ADEvalCase[A]) =
    new ADEval[Boolean => A => A => A] {
      override val fec = aInfo(boolInfo, aInfo(ai, aInfo(ai, ai)))

      override def term[G: Gradient] = base.ite(ai.wgi[G])
    }

  override def litB: Boolean => ADEval[Boolean] = b => new ADEval[Boolean] {
    override val fec = boolInfo

    override def term[G: Gradient] = base.litB(b)
  }

  override implicit def boolInfo: ADEvalCase.Aux[Boolean, Lambda[X => Boolean]] =
    new ADEvalCase[Boolean] with BoolRI[ADEvalMatch] {
      override type WithGrad[_] = Boolean

      override def wgi[G: Gradient]: LangInfoG[Boolean] = base.boolInfo
  }
}

object ADEvalBool extends ADEvalBool