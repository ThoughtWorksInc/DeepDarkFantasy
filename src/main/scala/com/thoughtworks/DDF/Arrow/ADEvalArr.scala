package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.{ADEval, ADEvalCase, ADEvalMatch}
import com.thoughtworks.DDF.LangBase.ADEvalLangBase
import com.thoughtworks.DDF.Language.{LangInfoG, LangTermLang}
import com.thoughtworks.DDF.RecursiveInfoMatch._

trait ADEvalArr extends Arr with ADEvalLangBase {
  val base = LangTermLang

  override def app[A, B]: ADEval[A => B] => ADEval[A] => ADEval[B] = f => x => new ADEval[B] {
    val dom = domInfo(f.fec)

    val rng = rngInfo(f.fec)

    override val fec : ADEvalCase.Aux[B, rng.WithGrad] = rng

    override def term[G: Gradient] = base.app(f.get[G](aInfo(dom, rng)))(x.get[G](dom))
  }

  override implicit def aInfo[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]):
  ADEvalCase.Aux[A => B, Lambda[X => ai.WithGrad[X] => bi.WithGrad[X]]] =
    new ADEvalCase[A => B] with ArrowRI[ADEvalMatch, ADEvalCase, A, B] {
      override type WithGrad[X] = ai.WithGrad[X] => bi.WithGrad[X]

      override def wgi[G: Gradient]: LangInfoG[ai.WithGrad[G] => bi.WithGrad[G]] = base.aInfo(ai.wgi[G], bi.wgi[G])

      override def tmr = (ai, bi)
    }

  override def domInfo[A, B]: ADEvalCase[A => B] => ADEvalCase[A] = _.get(AM[ADEvalMatch, ADEvalCase, A, B])._1

  override def rngInfo[A, B]: ADEvalCase[A => B] => ADEvalCase[B] = _.get(AM[ADEvalMatch, ADEvalCase, A, B])._2
}

object ADEvalArr extends ADEvalArr