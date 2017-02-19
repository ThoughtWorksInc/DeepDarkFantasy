package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.Language.InterLang

import scalaz.NaturalTransformation

trait ILIGTopType[R[_]] extends
  TopType[InterLangInfoG, R] {
  override implicit def topInfo: InterLangInfoG[Unit] = new InterLangInfoG[Unit] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Unit] = lang.topInfo
  }
}

object ILIGTopType {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => ILIGTopType[Repr] =
    nt => new ILIGTopType[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}