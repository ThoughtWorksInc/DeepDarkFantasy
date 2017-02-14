package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.LangTermArr
import com.thoughtworks.DDF.Language._
import com.thoughtworks.DDF.RecursiveInfoMatch.BoolRI

trait LangTermBool extends Bool[LangInfoG, LangTerm] with LangTermArr {
  override def ite[A](implicit ai: LangInfoG[A]) = new RawLangTerm[Boolean => A => A => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.ite[A](ai(lang))
  }.convert

  override def litB: Boolean => LangTerm[Boolean] = b => new RawLangTerm[Boolean] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.litB(b)
  }.convert

  override implicit def boolInfo: LangInfoG[Boolean] = new LangInfoG[Boolean] with BoolRI[LangInfoGMatch] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[Boolean] = lang.boolInfo
  }
}

object LangTermBool extends LangTermBool