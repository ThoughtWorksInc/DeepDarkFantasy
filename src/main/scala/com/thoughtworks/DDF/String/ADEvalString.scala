package com.thoughtworks.DDF.String

import com.thoughtworks.DDF.Arrow.ADEvalArr
import com.thoughtworks.DDF.{ADEval, ADEvalCase, ADEvalMatch}
import com.thoughtworks.DDF.Gradient.Gradient
import com.thoughtworks.DDF.InfoBase.ADEvalInfoBase
import com.thoughtworks.DDF.Language.{LangTermLang}
import com.thoughtworks.DDF.RecursiveInfoMatch.StringRI

trait ADEvalString extends String[ADEvalCase, ADEval] with ADEvalInfoBase with ADEvalArr {
  override val base = LangTermLang

  override def stringInfo: ADEvalCase.Aux[scala.Predef.String, Lambda[X => scala.Predef.String]] =
    new ADEvalCase[scala.Predef.String] with StringRI[ADEvalMatch] {
      override def wgi[G: Gradient] = base.stringInfo

      override type WithGrad[_] = scala.Predef.String
    }

  override def litString = str => new ADEval[scala.Predef.String] {
    override def term[G: Gradient] = base.litString(str)

    override val fec = stringInfo
  }

  override def stringApp = new ADEval[scala.Predef.String => scala.Predef.String => scala.Predef.String] {
    override def term[G: Gradient] = base.stringApp

    override val fec = aInfo(stringInfo, aInfo(stringInfo, stringInfo))
  }
}

object ADEvalString extends ADEvalString