package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.ILIGArrInfo
import com.thoughtworks.DDF.Language.{InterLang, InterLangInfoG}

import scalaz.NaturalTransformation

trait ILIGBoolInfo[R[_]] extends
  BoolType[InterLangInfoG, R] with
  ILIGArrInfo[R] {
  override implicit def boolInfo: InterLangInfoG[Boolean] = new InterLangInfoG[Boolean] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Boolean] = lang.boolInfo
  }
}

object ILIGBoolInfo {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => ILIGBoolInfo[Repr] =
    nt => new ILIGBoolInfo[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}