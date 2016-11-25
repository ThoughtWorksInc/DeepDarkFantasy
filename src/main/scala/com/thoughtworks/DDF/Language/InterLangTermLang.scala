package com.thoughtworks.DDF.Language

trait InterLangTermLang extends
  InterLang2Lang[InterLangInfoG, InterLangTerm] with
  Lang[InterLangInfoG, InterLangTerm] {
  override def i: InterLang[InterLangInfoG, InterLangTerm] = InterLangTermInterLang
}

object InterLangTermLang extends InterLangTermLang