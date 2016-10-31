package com.thoughtworks.DDF.Language

trait InterLangTerm[X] {
  def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Repr[X]
}
