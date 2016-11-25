package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.InfoBase.ADEvalInfoBase
import com.thoughtworks.DDF.Language.LangTermLang
import com.thoughtworks.DDF.{ADEval, ADEvalCase, ADEvalMatch}

trait ADEvalTop extends
  Top[ADEvalCase, ADEval] with
  ADEvalInfoBase {
  val base = LangTermLang

  override implicit def topInfo: ADEvalCase.Aux[Unit, Lambda[G => Unit]] =
    new ADEvalCase[Unit] {
      override type WithGrad[_] = Unit

      override val tm = new ADEvalMatch[Unit] {
        override type ret = Unit
      }

      override def tmr: tm.ret = ()

      override def wgi[G: Gradient] = base.topInfo
  }

  override def mkTop: ADEval[Unit] = new ADEval[Unit] {
    override val fec = topInfo

    override def term[G: Gradient] = base.mkTop
  }
}

object ADEvalTop extends ADEvalTop
