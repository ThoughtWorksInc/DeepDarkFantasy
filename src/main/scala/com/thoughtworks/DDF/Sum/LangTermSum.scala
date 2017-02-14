package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.LangTermArr
import com.thoughtworks.DDF.Language._
import com.thoughtworks.DDF.RecursiveInfoMatch.{SumRI, _}

trait LangTermSum extends Sum[LangInfoG, LangTerm] with LangTermArr {
  override def sumAssocRL[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[Either[A, Either[B, C]] => Either[Either[A, B], C]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.sumAssocRL(ai(lang), bi(lang), ci(lang))
    }.convert

  override def sumMatch[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[Either[A, B] => (A => C) => (B => C) => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.sumMatch(ai(lang), bi(lang), ci(lang))
    }.convert

  override implicit def sumInfo[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangInfoG[Either[A, B]] with SumRI[LangInfoGMatch, LangInfoG, A, B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.sumInfo(ai(lang), bi(lang))

      override def tmr: tm.ret = (ai, bi)
    }

  override def sumAssocLR[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[Either[Either[A, B], C] => Either[A, Either[B, C]]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.sumAssocLR(ai(lang), bi(lang), ci(lang))
    }.convert

  override def right[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): LangTerm[B => Either[A, B]] =
    new RawLangTerm[B => Either[A, B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.right[A, B](ai(lang), bi(lang))
    }.convert

  override def sumLeftInfo[A, B]: LangInfoG[Either[A, B]] => LangInfoG[A] =
    _.get(SM[LangInfoGMatch, LangInfoG, A, B])._1

  override def sumRightInfo[A, B]: LangInfoG[Either[A, B]] => LangInfoG[B] =
    _.get(SM[LangInfoGMatch, LangInfoG, A, B])._2

  override def sumComm[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[Either[A, B] => Either[B, A]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.sumComm[A, B](ai(lang), bi(lang))
    }.convert

  override def left[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[A => Either[A, B]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.left[A, B](ai(lang), bi(lang))
  }.convert
}

object LangTermSum extends LangTermSum