package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.InfoBase.LangTermInfoBase
import com.thoughtworks.DDF.Language.{Lang, LangInfoG, LangInfoGMatch, LangTerm}
import com.thoughtworks.DDF.RecursiveInfoMatch.{ArrowRI, _}

trait LangTermArr extends Arr[LangInfoG, LangTerm] with LangTermInfoBase {
  override def domInfo[A, B]: LangInfoG[A => B] => LangInfoG[A] = _.get(AM[LangInfoGMatch, LangInfoG, A, B])._1

  override def rngInfo[A, B]: LangInfoG[A => B] => LangInfoG[B] = _.get(AM[LangInfoGMatch, LangInfoG, A, B])._2

  override def app[A, B]: LangTerm[A => B] => LangTerm[A] => LangTerm[B] = f => x => new LangTerm[B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[B] = lang.app(f(lang))(x(lang))

    override def info: LangInfoG[B] = rngInfo(f.info)
  }

  override implicit def aInfo[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangInfoG[A => B] with ArrowRI[LangInfoGMatch, LangInfoG, A, B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.aInfo(ai(lang), bi(lang))

      override def tmr: tm.ret = (ai, bi)
    }
}

object LangTermArr extends LangTermArr