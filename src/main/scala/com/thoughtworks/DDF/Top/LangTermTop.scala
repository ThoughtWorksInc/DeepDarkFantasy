package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.LangBase.LangTermLangBase
import com.thoughtworks.DDF.Language._
import com.thoughtworks.DDF.RecursiveInfoMatch.TopRI

trait LangTermTop extends Top with LangTermLangBase {
  override implicit def topInfo: LangInfoG[Unit] = new LangInfoG[Unit] with TopRI[LangInfoGMatch] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[Unit] = lang.topInfo
  }

  override def mkTop: LangTerm[Unit] = new RawLangTerm[Unit] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.mkTop
  }.convert
}

object LangTermTop extends LangTermTop