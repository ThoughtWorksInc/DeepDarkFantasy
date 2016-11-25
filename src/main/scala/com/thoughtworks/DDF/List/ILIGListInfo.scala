package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.ILIGArrInfo
import com.thoughtworks.DDF.Language.{InterLang, InterLangInfoG}

import scalaz.NaturalTransformation

trait ILIGListInfo[R[_]] extends
  ListInfo[InterLangInfoG, R] with
  ILIGArrInfo[R] {
  override implicit def listInfo[A](implicit ai: InterLangInfoG[A]) = new InterLangInfoG[scala.List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[scala.List[A]] =
      lang.listInfo(ai(lang))
  }

  override def listElmInfo[A]: InterLangInfoG[scala.List[A]] => InterLangInfoG[A] = i => new InterLangInfoG[A] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.listElmInfo(i(lang))
  }
}

object ILIGListInfo {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => ILIGListInfo[Repr] =
    nt => new ILIGListInfo[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}