package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.Language.{InterLang, InterLangInfoG}

import scalaz.NaturalTransformation

trait ILIGTopInfo[R[_]] extends
  TopInfo[InterLangInfoG, R] {
  override implicit def topInfo: InterLangInfoG[Unit] = new InterLangInfoG[Unit] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Unit] = lang.topInfo
  }
}

object ILIGTopInfo {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => ILIGTopInfo[Repr] =
    nt => new ILIGTopInfo[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}