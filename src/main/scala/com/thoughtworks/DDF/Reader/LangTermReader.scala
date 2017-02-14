package com.thoughtworks.DDF.Reader

import com.thoughtworks.DDF.Arrow.LangTermArr
import com.thoughtworks.DDF.Language.{Lang, LangInfoG, LangTerm, RawLangTerm}

trait LangTermReader extends Reader[LangInfoG, LangTerm] with LangTermArr {
  override def readerBind[E, A, B](implicit ei: LangInfoG[E], ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[Reader[E, A] => (A => Reader[E, B]) => Reader[E, B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.readerBind(ei(lang), ai(lang), bi(lang))
    }.convert

  override def readerRet[E, A](implicit ei: LangInfoG[E], ai: LangInfoG[A]) = new RawLangTerm[A => Reader[E, A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.readerRet(ei(lang), ai(lang))
  }.convert
}

object LangTermReader extends LangTermReader