package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.InfoBase.FEvalInfoBase
import com.thoughtworks.DDF.Language.LangTermLang
import com.thoughtworks.DDF.{FEval, FEvalCase, FEvalMatch}

trait FEvalTop extends
  Top[FEvalCase, FEval] with
  FEvalInfoBase {
  val base = LangTermLang

  override implicit def topInfo: FEvalCase.Aux[Unit, Lambda[G => Unit]] =
    new FEvalCase[Unit] {
      override type WithGrad[_] = Unit

      override val tm = new FEvalMatch[Unit] {
        override type ret = Unit
      }

      override def tmr: tm.ret = ()

      override def wgi[G: Gradient] = base.topInfo
  }

  override def mkTop: FEval[Unit] = new FEval[Unit] {
    override val fec = topInfo

    override def term[G: Gradient] = base.mkTop
  }
}

object FEvalTop extends FEvalTop
