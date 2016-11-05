package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.Arrow.FEvalArr
import com.thoughtworks.DDF.Language.LangInfoG
import com.thoughtworks.DDF.{FEMMatch, FEval, FEvalCase}

trait FEvalBotMin[G] extends
  BotMin[FEvalCase[G, ?], FEval[G, ?]] with
  FEvalArr[G] {
  override implicit def botInfo: FEvalCase.Aux[G, Nothing, Nothing] =
    new FEvalCase[G, Nothing] {
      override type ret = Nothing

      override def lr: LangInfoG[Nothing] = base.botInfo

      override val tm = new FEMMatch[G, Nothing] {
        override type ret = Unit
      }

      override def tmr: tm.ret = ()
    }

  override def exfalso[A](implicit ai: FEvalCase[G, A]) =
    new FEval[G, Nothing => A] {
      override val tm = aInfo[Nothing, A](botInfo, ai)

      override val deriv = base.exfalso(ai.lr)
    }
}

object FEvalBotMin {
  implicit def apply[G] = new FEvalBotMin[G] { }
}