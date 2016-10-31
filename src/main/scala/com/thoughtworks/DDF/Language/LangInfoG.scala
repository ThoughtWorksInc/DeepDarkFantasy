package com.thoughtworks.DDF.Language

trait LangInfoG[X] {
  def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[X]
}
