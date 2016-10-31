package com.thoughtworks.DDF.Language

trait InterLangTermLang extends
  InterLang2Lang[InterLangInfoG, InterLangTerm] with
  Lang[InterLangInfoG, InterLangTerm] {
  override def i: InterLang[InterLangInfoG, InterLangTerm] = InterLangTermInterLang
}

object InterLangTermLang extends InterLangTermLang

import scalaz.Isomorphism._

trait LangTermLang extends Lang[LangInfoG, LangTerm] with IsoLang[InterLangInfoG, LangInfoG, InterLangTerm, LangTerm] {
  override def infoIso: InterLangInfoG <~> LangInfoG = ???

  override def reprIso: InterLangTerm <~> LangTerm = ???

  override def l: Lang[InterLangInfoG, InterLangTerm] = ???
}

object LangTermLang extends LangTermLang