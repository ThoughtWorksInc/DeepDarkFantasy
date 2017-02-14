package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.LangTermArr
import com.thoughtworks.DDF.Language._
import com.thoughtworks.DDF.RecursiveInfoMatch._

trait LangTermProd extends Prod[LangInfoG, LangTerm] with LangTermArr {
  override def mkProd[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[A => B => (A, B)] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.mkProd(ai(lang), bi(lang))
  }.convert

  override def ><[A, B, C, D](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C], di: LangInfoG[D]) =
    new RawLangTerm[(A => C) => (B => D) => ((A, B)) => (C, D)] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.`><`(ai(lang), bi(lang), ci(lang), di(lang))
    }.convert

  override def zro[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[((A, B)) => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.zro(ai(lang), bi(lang))
  }.convert

  override def uncurry[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[(A => B => C) => ((A, B)) => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.uncurry(ai(lang), bi(lang), ci(lang))
    }.convert

  override implicit def prodInfo[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangInfoG[(A, B)] with ProductRI[LangInfoGMatch, LangInfoG, A, B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.prodInfo[A, B](ai(lang), bi(lang))

      override def tmr: tm.ret = (ai, bi)
    }

  override def prodZroInfo[A, B]: LangInfoG[(A, B)] => LangInfoG[A] = _.get(PM[LangInfoGMatch, LangInfoG, A, B])._1

  override def prodFstInfo[A, B]: LangInfoG[(A, B)] => LangInfoG[B] = _.get(PM[LangInfoGMatch, LangInfoG, A, B])._2

  override def fst[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[((A, B)) => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.fst(ai(lang), bi(lang))
  }.convert

  override def curry[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[(((A, B)) => C) => A => B => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.curry(ai(lang), bi(lang), ci(lang))
    }.convert
}

object LangTermProd extends LangTermProd