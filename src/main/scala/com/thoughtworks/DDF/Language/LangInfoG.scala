package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.TypeCase

trait LangInfoG[X] extends TypeCase[LangInfoGMatch[X]] {
  def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[X]
}
