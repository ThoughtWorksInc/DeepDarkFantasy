package com.thoughtworks.DDF.Language

trait LangTerm[X] {
  def info: LangInfoG[X]
  def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[X]
}
