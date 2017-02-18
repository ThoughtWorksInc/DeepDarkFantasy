package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.Language.{InterLang, InterLangInfoG}

import scalaz.NaturalTransformation

trait ILIGBotInfo[R[_]] extends
  BotType[InterLangInfoG, R] {
  override implicit def botInfo: InterLangInfoG[Nothing] =
    new InterLangInfoG[Nothing] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Nothing] = lang.botInfo
    }
}

object ILIGBotInfo {
  def apply[Repr[_]]: NaturalTransformation[Repr, InterLangInfoG] => ILIGBotInfo[Repr] =
    nt => new ILIGBotInfo[Repr] {
      override def reprInfo[A]: Repr[A] => InterLangInfoG[A] = nt.apply
    }
}