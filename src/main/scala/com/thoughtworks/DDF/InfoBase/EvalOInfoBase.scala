package com.thoughtworks.DDF.InfoBase

import com.thoughtworks.DDF.Language.InterLangInfoG
import com.thoughtworks.DDF.{EvalO, EvalOBase}

trait EvalOInfoBase extends EvalOBase with InfoBase[InterLangInfoG, EvalO] {
  override def reprInfo[A]: EvalO[A] => InterLangInfoG[A] = x => ltl.reprInfo(x.l)
}

object EvalOInfoBase extends EvalOInfoBase