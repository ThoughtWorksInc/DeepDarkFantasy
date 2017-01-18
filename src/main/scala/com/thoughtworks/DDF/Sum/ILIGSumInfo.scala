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
}

object ILIGSumInfo {
  def apply[Repr[_]]: ILIGSumInfo[Repr] = new ILIGSumInfo[Repr] { }
}