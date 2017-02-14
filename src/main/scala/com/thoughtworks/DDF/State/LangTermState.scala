package com.thoughtworks.DDF.State

import com.thoughtworks.DDF.Language.{Lang, LangInfoG, LangTerm, RawLangTerm}
import com.thoughtworks.DDF.Product.LangTermProd

trait LangTermState extends State[LangInfoG, LangTerm] with LangTermProd {
  override def stateBind[S, A, B](implicit si: LangInfoG[S], ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[State[S, A] => (A => State[S, B]) => State[S, B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.stateBind(si(lang), ai(lang), bi(lang))
    }.convert

  override def stateRet[S, A](implicit si: LangInfoG[S], ai: LangInfoG[A]) = new RawLangTerm[A => State[S, A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.stateRet[S, A](si(lang), ai(lang))
  }.convert
}

object LangTermState extends LangTermState