package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.Language.{InterLang, InterLangInfoG}

import scalaz.NaturalTransformation

trait ILIGArrInfo[R[_]] extends ArrInfo[InterLangInfoG, R] {
  override implicit def aInfo[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangInfoG[A => B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.aInfo(ai(lang), bi(lang))
    }
}

object ILIGArrInfo {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => ILIGArrInfo[Repr] =
    nt => new ILIGArrInfo[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}