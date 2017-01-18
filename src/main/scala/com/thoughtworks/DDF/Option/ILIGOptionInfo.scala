package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.ILIGArrInfo
import com.thoughtworks.DDF.Language.{InterLang, InterLangInfoG}

import scalaz.NaturalTransformation

trait ILIGOptionInfo[R[_]] extends
  OptionInfo[InterLangInfoG, R] with
  ILIGArrInfo[R] {
  override implicit def optionInfo[A](implicit ai: InterLangInfoG[A]) = new InterLangInfoG[scala.Option[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.optionInfo(ai(lang))
  }
}

object ILIGOptionInfo {
  def apply[Repr[_]]: ILIGOptionInfo[Repr] = new ILIGOptionInfo[Repr] { }
}