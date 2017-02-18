package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.LangTermArr
import com.thoughtworks.DDF.Language._
import com.thoughtworks.DDF.RecursiveInfoMatch._

trait LangTermOption extends Option with LangTermArr {
  override def some[A](implicit ai: LangInfoG[A]) = new RawLangTerm[A => scala.Option[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.some[A](ai(lang))
  }.convert

  override def none[A](implicit ai: LangInfoG[A]) = new RawLangTerm[scala.Option[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[scala.Option[A]] =
      lang.none[A](ai(lang))
  }.convert

  override def optionElmInfo[A]: LangInfoG[scala.Option[A]] => LangInfoG[A] = _.get(OM[LangInfoGMatch, LangInfoG, A])

  override def optionMatch[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[scala.Option[A] => B => (A => B) => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.optionMatch(ai(lang), bi(lang))
    }.convert

  override implicit def optionInfo[A](implicit ai: LangInfoG[A]) =
    new LangInfoG[scala.Option[A]] with OptionRI[LangInfoGMatch, LangInfoG, A] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.optionInfo(ai(lang))

      override def tmr: tm.ret = ai
    }
}

object LangTermOption extends LangTermOption
