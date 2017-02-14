package com.thoughtworks.DDF.Cont

import com.thoughtworks.DDF.Arrow.LangTermArr
import com.thoughtworks.DDF.Language.{Lang, LangInfoG, LangTerm, RawLangTerm}

trait LangTermCont extends Cont[LangInfoG, LangTerm] with LangTermArr {
  override def contRet[R, A](implicit ri: LangInfoG[R], ai: LangInfoG[A]) = new RawLangTerm[A => Cont[R, A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.contRet(ri(lang), ai(lang))
  }.convert

  override def contBind[R, A, B](implicit ri: LangInfoG[R], ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[Cont[R, A] => (A => Cont[R, B]) => Cont[R, B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.contBind(ri(lang), ai(lang), bi(lang))
    }.convert
}

object LangTermCont extends LangTermCont