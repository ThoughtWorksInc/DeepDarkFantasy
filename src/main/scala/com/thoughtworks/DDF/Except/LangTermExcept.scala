package com.thoughtworks.DDF.Except

import com.thoughtworks.DDF.Language.{Lang, LangInfoG, LangTerm, RawLangTerm}
import com.thoughtworks.DDF.Sum.LangTermSum

trait LangTermExcept extends Except with LangTermSum {
  override def exceptBind[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[Except[A, B] => (B => Except[A, C]) => Except[A, C]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.exceptBind(ai(lang), bi(lang), ci(lang))
    }.convert
}

object LangTermExcept extends LangTermExcept