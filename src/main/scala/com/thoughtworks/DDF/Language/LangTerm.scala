package com.thoughtworks.DDF.Language

trait LangTerm[X] {
  def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Repr[X]
}
