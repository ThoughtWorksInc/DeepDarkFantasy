package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.Arrow.LangTermArr
import com.thoughtworks.DDF.Language._
import com.thoughtworks.DDF.RecursiveInfoMatch._
import com.thoughtworks.DDF.Top.LangTermTop

trait LangTermBot extends Bot with LangTermTop with LangTermArr {
  override implicit def botInfo = new LangInfoG[Nothing] with BotRI[LangInfoGMatch] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[Nothing] = lang.botInfo
  }

  override def exfalso[A](implicit ai: LangInfoG[A]) = new LangTerm[Nothing => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.exfalso[A](ai(lang))

    override def info: LangInfoG[Nothing => A] = aInfo[Nothing, A]
  }

  override def imfalso[A](implicit ai: LangInfoG[A]) = new RawLangTerm[Unit => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.imfalso[A](ai(lang))
  }.convert

  override def impossible = new LangTerm[Unit => Nothing] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.impossible

    override def info: LangInfoG[Unit => Nothing] = aInfo[Unit, Nothing]
  }
}

object LangTermBot extends LangTermBot
