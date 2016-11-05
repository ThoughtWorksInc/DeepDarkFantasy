package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.InfoBase.{FEvalInfoBase, InfoBase}
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm, LangTermLang}
import com.thoughtworks.DDF.{FEMMatch, FEval, FEvalCase}





trait FEvalArr[G] extends Arr[FEvalCase[G, ?], FEval[G, ?]] with FEvalInfoBase[G] {
  val base = LangTermLang

  override def app[A, B]: FEval[G, A => B] => FEval[G, A] => FEval[G, B] = f => x => new FEval[G, B] {
    val ai = domInfo(f.tm)

    val bi = rngInfo(f.tm)

    override val tm: FEvalCase.Aux[G, B, bi.ret] = bi

    override val deriv: LangTerm[tm.ret] = base.app(f.get(aInfo(ai, bi)))(x.get(ai))
  }

  def afem[A, B] = new FEMMatch[G, A => B] {
    override type ret = (FEvalCase[G, A], FEvalCase[G, B])
  }

  override implicit def aInfo[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]):
  FEvalCase.Aux[G, A => B, ai.ret => bi.ret] =
    new FEvalCase[G, A => B] {
      override type ret = ai.ret => bi.ret

      override val tm = afem[A, B]

      override def tmr: tm.ret = (ai, bi)

      override def lr: LangInfoG[ai.ret => bi.ret] = base.aInfo(ai.lr, bi.lr)
    }

  override def domInfo[A, B]: FEvalCase[G, A => B] => FEvalCase[G, A] = _.get(afem[A, B])._1

  override def rngInfo[A, B]: FEvalCase[G, A => B] => FEvalCase[G, B] = _.get(afem[A, B])._2
}

object FEvalArr {
  implicit def apply[G]: FEvalArr[G] = new FEvalArr[G] { }
}