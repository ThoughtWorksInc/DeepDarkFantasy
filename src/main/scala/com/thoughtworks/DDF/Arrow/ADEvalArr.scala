package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.{ADEval, ADEvalCase, ADEvalMatch}
import com.thoughtworks.DDF.Language.{LangInfoG, LangTermLang}

trait ADEvalArr extends Arr[ADEvalCase, ADEval] {
  val base = LangTermLang

  override def app[A, B]: ADEval[A => B] => ADEval[A] => ADEval[B] = f => x => new ADEval[B] {
    val dom = domInfo(f.fec)

    val rng = rngInfo(f.fec)

    override val fec : ADEvalCase.Aux[B, rng.WithGrad] = rng

    override def term[G: Gradient] = {
      implicit val gi = implicitly[Gradient[G]].GInfo
      implicit val domgi = dom.wgi[G]
      implicit val rnggi = rng.wgi[G]
      base.app(f.get[G](aInfo(dom, rng)))(x.get[G](dom))
    }
  }

  def afem[A, B] = new ADEvalMatch[A => B] {
    override type ret = (ADEvalCase[A], ADEvalCase[B])
  }

  override implicit def aInfo[A, B](implicit ai: ADEvalCase[A], bi: ADEvalCase[B]):
  ADEvalCase.Aux[A => B, Lambda[X => ai.WithGrad[X] => bi.WithGrad[X]]] =
    new ADEvalCase[A => B] {
      override type WithGrad[X] = ai.WithGrad[X] => bi.WithGrad[X]

      override def wgi[G: Gradient]: LangInfoG[ai.WithGrad[G] => bi.WithGrad[G]] = base.aInfo(ai.wgi[G], bi.wgi[G])

      override val tm = afem[A, B]

      override def tmr = (ai, bi)
    }

  def domInfo[A, B]: ADEvalCase[A => B] => ADEvalCase[A] = _.get(afem[A, B])._1

  def rngInfo[A, B]: ADEvalCase[A => B] => ADEvalCase[B] = _.get(afem[A, B])._2
}

object ADEvalArr extends ADEvalArr