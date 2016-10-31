package com.thoughtworks.DDF

import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}

trait FEMMatch[X] extends TypeMatch[FEMMatch, X]

object FEMMatch {
  type Aux[X, XM] = FEMMatch[X] {type ret = XM}
}

trait FEvalMatch[X] extends TypeMatch[FEvalMatch, X] with TypeCase[FEMMatch, X] {
  def selfG: LangTerm[X => ret]

  def gSelf: LangTerm[ret => X]

  def LI: LangInfoG[X]
}

object FEvalMatch {
  type Aux[X, XM] = FEvalMatch[X] {type ret = XM}
}

trait FEval[X] {
  val tm: FEvalMatch[X]

  val deriv: LangTerm[tm.ret]

  def get[XM](implicit fm: FEvalMatch.Aux[X, XM]): LangTerm[XM] = tm.unique(fm).subst(deriv)
}
