package com.thoughtworks.DDF.LangBase

import com.thoughtworks.DDF.Language.{LangInfoG, LangTerm}

trait LangTermLangBase extends LangBase {
  override def reprInfo[A]: LangTerm[A] => LangInfoG[A] = _.info
}

object LangTermLangBase extends LangTermLangBase