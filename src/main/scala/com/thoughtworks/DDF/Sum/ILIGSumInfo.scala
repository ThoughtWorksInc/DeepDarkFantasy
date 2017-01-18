package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.ILIGArrInfo
import com.thoughtworks.DDF.Language.{InterLang, InterLangInfoG}

import scalaz.NaturalTransformation

trait ILIGSumInfo[R[_]] extends
  SumInfo[InterLangInfoG, R] with
  ILIGArrInfo[R] {
  override implicit def sumInfo[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangInfoG[Either[A, B]] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Either[A, B]] =
        lang.sumInfo(ai(lang), bi(lang))
    }

  override def sumLeftInfo[A, B]: InterLangInfoG[Either[A, B]] => InterLangInfoG[A] = i =>
    new InterLangInfoG[A] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.sumLeftInfo(i(lang))
    }

  override def sumRightInfo[A, B]: InterLangInfoG[Either[A, B]] => InterLangInfoG[B] = i =>
    new InterLangInfoG[B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[B] = lang.sumRightInfo(i(lang))
    }
}

object ILIGSumInfo {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => ILIGSumInfo[Repr] =
    nt => new ILIGSumInfo[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}