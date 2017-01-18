package com.thoughtworks.DDF.Language

trait RawLangTerm[X] { self =>
  def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[X]
  def convert(implicit xi: LangInfoG[X]): LangTerm[X] = new LangTerm[X] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[X] = self.apply[Info, Repr]

    override def info: LangInfoG[X] = xi
  }
}
