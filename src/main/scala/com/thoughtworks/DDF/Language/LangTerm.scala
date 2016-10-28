package com.thoughtworks.DDF.Language

trait LangInfoG[X] {
  def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[X]
}

trait LangTerm[X] {
  def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[X]
}
