package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.Language.{InterLang, InterLangInfoG}

import scalaz.NaturalTransformation

trait ILIGArrInfo[R[_]] extends ArrInfo[InterLangInfoG, R] {
  override implicit def aInfo[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangInfoG[A => B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.aInfo(ai(lang), bi(lang))
    }

  override def domInfo[A, B]: InterLangInfoG[A => B] => InterLangInfoG[A] = i =>
    new InterLangInfoG[A] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.domInfo(i(lang))
    }

  override def rngInfo[A, B]: InterLangInfoG[A => B] => InterLangInfoG[B] = i =>
    new InterLangInfoG[B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[B] = lang.rngInfo(i(lang))
    }
}

object ILIGArrInfo {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => ILIGArrInfo[Repr] =
    nt => new ILIGArrInfo[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}