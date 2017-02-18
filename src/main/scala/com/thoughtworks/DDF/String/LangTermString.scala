package com.thoughtworks.DDF.String

import com.thoughtworks.DDF.Arrow.LangTermArr
import com.thoughtworks.DDF.Language._
import com.thoughtworks.DDF.RecursiveInfoMatch._

trait LangTermString extends String with LangTermArr {
  override def litString = str => new RawLangTerm[scala.Predef.String] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[scala.Predef.String] =
      lang.litString(str)
  }.convert

  override implicit def stringInfo: LangInfoG[scala.Predef.String] =
    new LangInfoG[scala.Predef.String] with StringRI[LangInfoGMatch] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[scala.Predef.String] = lang.stringInfo
    }

  override def stringApp = new RawLangTerm[scala.Predef.String => scala.Predef.String => scala.Predef.String] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.stringApp
  }.convert
}

object LangTermString extends LangTermString