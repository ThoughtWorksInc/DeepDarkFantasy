package com.thoughtworks.DDF.LangBase

import com.thoughtworks.DDF.{EvalO, EvalOBase}

trait EvalOLangBase extends EvalOBase with LangBase {
  override def reprInfo[A]: EvalO[A] => InterLangInfoG[A] = x => ltl.reprInfo(x.l)
}

object EvalOLangBase extends EvalOLangBase