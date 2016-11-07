package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.{FEval, FEvalCase, FEvalMatch, Gradient}
import com.thoughtworks.DDF.InfoBase.FEvalInfoBase
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm, LangTermLang}

trait FEvalArr extends Arr[FEvalCase, FEval] with FEvalInfoBase {
  val base = LangTermLang

  override def app[A, B]: FEval[A => B] => FEval[A] => FEval[B] = f => x => new FEval[B] {
    val dom = domInfo(f.fec)

    val rng = rngInfo(f.fec)

    override val fec : FEvalCase.Aux[B, rng.WithGrad] = rng

    override def term[G: Gradient] = base.app(f.get[G](aInfo(dom, rng)))(x.get[G](dom))
  }

  def afem[A, B] = new FEvalMatch[A => B] {
    override type ret = (FEvalCase[A], FEvalCase[B])
  }

  override implicit def aInfo[A, B](implicit ai: FEvalCase[A], bi: FEvalCase[B]):
  FEvalCase.Aux[A => B, Lambda[X => ai.WithGrad[X] => bi.WithGrad[X]]] =
    new FEvalCase[A => B] {
      override type WithGrad[X] = ai.WithGrad[X] => bi.WithGrad[X]

      override def wgi[G: Gradient]: LangInfoG[ai.WithGrad[G] => bi.WithGrad[G]] = base.aInfo(ai.wgi[G], bi.wgi[G])

      override val tm = afem[A, B]

      override def tmr = (ai, bi)
    }

  override def domInfo[A, B]: FEvalCase[A => B] => FEvalCase[A] = _.get(afem[A, B])._1

  override def rngInfo[A, B]: FEvalCase[A => B] => FEvalCase[B] = _.get(afem[A, B])._2
}

object FEvalArr extends FEvalArr