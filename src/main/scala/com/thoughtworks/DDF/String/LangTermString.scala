package com.thoughtworks.DDF.String

import com.thoughtworks.DDF.Arrow.LangTermArr
import com.thoughtworks.DDF.Language._

trait LangTermString extends com.thoughtworks.DDF.String.String[LangInfoG, LangTerm] with LangTermArr {
  override def litString: String => LangTerm[String] = str => new RawLangTerm[String] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[String] = lang.litString(str)
  }.convert

  override implicit def stringInfo: LangInfoG[String] = new LangInfoG[String] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[String] = lang.stringInfo

    override val tm: LangInfoGMatch.Aux[String, Unit] = new LangInfoGMatch[String] {
      override type ret = Unit
    }

    override def tmr: tm.ret = ()
  }

  override def stringApp = new RawLangTerm[String => String => String] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.stringApp
  }.convert
}

object LangTermString extends LangTermString