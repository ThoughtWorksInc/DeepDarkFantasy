package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ILIGArrInfo
import com.thoughtworks.DDF.Language.InterLang

import scalaz.NaturalTransformation

trait ILIGDoubleInfo[R[_]] extends
  DoubleType[InterLangInfoG, R] with
  ILIGArrInfo[R] {
  override implicit def doubleInfo: InterLangInfoG[scala.Double] = new InterLangInfoG[scala.Double] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[scala.Double] = lang.doubleInfo
  }
}

object ILIGDoubleInfo {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => ILIGDoubleInfo[Repr] =
    nt => new ILIGDoubleInfo[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}