package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.ILIGArrInfo
import com.thoughtworks.DDF.Language.{InterLang, InterLangInfoG}

import scalaz.NaturalTransformation

trait ILIGProdType[R[_]] extends
  ProdType[InterLangInfoG, R] with
  ILIGArrInfo[R] {
  override implicit def prodInfo[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangInfoG[(A, B)] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[(A, B)] =
        lang.prodInfo(ai(lang), bi(lang))
    }

  override def prodZroInfo[A, B]: InterLangInfoG[(A, B)] => InterLangInfoG[A] = i =>
    new InterLangInfoG[A] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.prodZroInfo(i(lang))
    }

  override def prodFstInfo[A, B]: InterLangInfoG[(A, B)] => InterLangInfoG[B] = i =>
    new InterLangInfoG[B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[B] = lang.prodFstInfo(i(lang))
    }
}

object ILIGProdType {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => ILIGProdType[Repr] =
    nt => new ILIGProdType[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}
