package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.InfoBase.EvalOInfoBase
import com.thoughtworks.DDF.Language.{InterLangInfoG, InterLangTerm}
import com.thoughtworks.DDF.{EvalO, EvalOBase, EvalOMatch}

trait EvalOArr extends
  EvalOBase with
  Arr[InterLangInfoG, EvalO] with
  ILIGArrInfo[EvalO] with
  EvalOInfoBase {
  def aeval[A, B]: InterLangTerm[A => B] => (EvalO[A] => EvalO[B]) => EvalO[A => B] = la => f =>
    new EvalO[A => B] {
      override def l: InterLangTerm[A => B] = la

      override def tmr: tm.ret = f

      override val tm: EvalOMatch.Aux[A => B, EvalO[A] => EvalO[B]] = AEM[A, B]
    }

  override def app[A, B]: EvalO[A => B] => EvalO[A] => EvalO[B] = f => f.get(AEM[A, B])

  def AEM[A, B]: EvalOMatch.Aux[A => B, EvalO[A] => EvalO[B]] = new EvalOMatch[A => B] {
    override type ret = EvalO[A] => EvalO[B]
  }
}

object EvalOArr extends EvalOArr