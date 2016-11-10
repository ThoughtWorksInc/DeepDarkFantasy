package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.Arrow.FEvalArr
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.Language.LangInfoG
import com.thoughtworks.DDF.{FEval, FEvalCase, FEvalMatch}

trait FEvalBotMin extends
  BotMin[FEvalCase, FEval] with
  FEvalArr {
  override implicit def botInfo: FEvalCase.Aux[Nothing, Lambda[X => Nothing]] =
    new FEvalCase[Nothing] {
      override type WithGrad[_] = Nothing

      override def wgi[G: Gradient]: LangInfoG[Nothing] = base.botInfo

      override val tm = new FEvalMatch[Nothing] {
        override type ret = Unit
      }

      override def tmr: tm.ret = ()
    }

  override def exfalso[A](implicit ai: FEvalCase[A]) =
    new FEval[Nothing => A] {
      override val fec = aInfo[Nothing, A](botInfo, ai)

      override def term[G: Gradient] = base.exfalso(ai.wgi[G])
    }
}

object FEvalBotMin extends FEvalBotMin