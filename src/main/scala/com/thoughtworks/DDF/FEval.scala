package com.thoughtworks.DDF

import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}

trait FEMMatch[G, X] extends TypeMatch[FEMMatch[G, ?], X]

object FEMMatch {
  type Aux[G, X, XM] = FEMMatch[G, X] {type ret = XM}
}

trait FEvalCase[G, X] extends TypeMatch[FEvalCase[G, ?], X] with TypeCase[FEMMatch[G, ?], X] {
  def lr: LangInfoG[ret]
}

object FEvalCase {
  type Aux[G, X, XM] = FEvalCase[G, X] {type ret = XM}
}

trait FEval[G, X] {
  val tm: FEvalCase[G, X]

  val deriv: LangTerm[tm.ret]

  def get[XM](implicit fm: FEvalCase.Aux[G, X, XM]): LangTerm[XM] = tm.unique(fm).subst(deriv)
}
