package com.thoughtworks.DDF.LangBase

import com.thoughtworks.DDF.Language.InterLangInfoG
import com.thoughtworks.DDF.{EvalO, EvalOBase}

trait EvalOLangBase extends EvalOBase with LangBase[InterLangInfoG, EvalO] {
  override def reprInfo[A]: EvalO[A] => InterLangInfoG[A] = x => ltl.reprInfo(x.l)
}

object EvalOLangBase extends EvalOLangBase