package com.thoughtworks.DDF.InfoBase

import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}

trait LangTermInfoBase extends InfoBase[LangInfoG, LangTerm] {
  override def reprInfo[A]: LangTerm[A] => LangInfoG[A] = _.info
}

object LangTermInfoBase extends LangTermInfoBase