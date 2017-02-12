package com.thoughtworks.DDF.String

import com.thoughtworks.DDF.Arrow.EvalMArr
import com.thoughtworks.DDF.Language.SimpleLang
import com.thoughtworks.DDF.NoInfo

trait EvalMString extends String[NoInfo, Lambda[X => X]] with EvalMArr with SimpleLang[Lambda[X => X]] {
  override def litString: scala.Predef.String => scala.Predef.String = identity[scala.Predef.String]

  override def stringApp: scala.Predef.String => scala.Predef.String => scala.Predef.String = l => r => l + r
}

object EvalMString extends EvalMString