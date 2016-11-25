package com.thoughtworks.DDF.Language
import com.thoughtworks.DDF.ADEvalCase
import com.thoughtworks.DDF.Gradient.Gradient

trait LangTermDLang extends DLang[LangInfoG, LangTerm] with LangTermLang {
  override def diff[G: Gradient, A]:
  LangTerm[A] => LangTerm[fec.WithGrad[G]] forSome { val fec : ADEvalCase[A] } = a =>
    a(InterLang2Lang.apply(ADEvalInterLang)).term
}

object LangTermDLang extends LangTermDLang