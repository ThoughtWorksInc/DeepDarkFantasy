package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.Arrow.ADEvalArr
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.Language.LangInfoG
import com.thoughtworks.DDF.{ADEval, ADEvalCase, ADEvalMatch}
import com.thoughtworks.DDF.RecursiveInfoMatch._

trait ADEvalBotMin extends
  BotMin[ADEvalCase, ADEval] with
  ADEvalArr {
  override implicit def botInfo: ADEvalCase.Aux[Nothing, Lambda[X => Nothing]] =
    new ADEvalCase[Nothing] with BotRI[ADEvalMatch] {
      override type WithGrad[_] = Nothing

      override def wgi[G: Gradient]: LangInfoG[Nothing] = base.botInfo
    }

  override def exfalso[A](implicit ai: ADEvalCase[A]) =
    new ADEval[Nothing => A] {
      override val fec = aInfo[Nothing, A](botInfo, ai)

      override def term[G: Gradient] = base.exfalso(ai.wgi[G])
    }
}

object ADEvalBotMin extends ADEvalBotMin