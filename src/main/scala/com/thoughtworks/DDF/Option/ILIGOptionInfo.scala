package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.ILIGArrInfo
import com.thoughtworks.DDF.Language.InterLang

import scalaz.NaturalTransformation

trait ILIGOptionInfo[R[_]] extends
  OptionType[InterLangInfoG, R] with
  ILIGArrInfo[R] {
  override implicit def optionInfo[A](implicit ai: InterLangInfoG[A]) = new InterLangInfoG[scala.Option[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.optionInfo(ai(lang))
  }

  override def optionElmInfo[A]: InterLangInfoG[scala.Option[A]] => InterLangInfoG[A] = i => new InterLangInfoG[A] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.optionElmInfo(i(lang))
  }
}

object ILIGOptionInfo {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => ILIGOptionInfo[Repr] =
    nt => new ILIGOptionInfo[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}