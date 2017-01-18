package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.ILIGArrInfo
import com.thoughtworks.DDF.Language.{InterLang, InterLangInfoG}

import scalaz.NaturalTransformation

trait ILIGProdInfo[R[_]] extends
  ProdInfo[InterLangInfoG, R] with
  ILIGArrInfo[R] {
  override implicit def prodInfo[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangInfoG[(A, B)] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[(A, B)] =
        lang.prodInfo(ai(lang), bi(lang))
    }
}

object ILIGProdInfo {
  def apply[Repr[_]]: ILIGProdInfo[Repr] = new ILIGProdInfo[Repr] { }
}
