package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.InfoBase.FEvalInfoBase
import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm, LangTermLang}
import com.thoughtworks.DDF.{FEMMatch, FEval, FEvalCase}

trait FEvalTop[G] extends
  Top[FEvalCase[G, ?], FEval[G, ?]] with
  FEvalInfoBase[G] {
  val base = LangTermLang

  override implicit def topInfo: FEvalCase.Aux[G, Unit, Unit] = new FEvalCase[G, Unit] {
    override type ret = Unit

    override val tm: FEMMatch.Aux[G, Unit, Unit] = new FEMMatch[G, Unit] {
      override type ret = Unit
    }

    override def tmr: tm.ret = ()

    override def lr: LangInfoG[Unit] = base.topInfo
  }

  override def mkTop: FEval[G, Unit] = new FEval[G, Unit] {
    override val tm = topInfo

    override val deriv: LangTerm[tm.ret] = base.mkTop
  }
}

object FEvalTop {
  implicit def apply[G]: FEvalTop[G] = new FEvalTop[G] { }
}
