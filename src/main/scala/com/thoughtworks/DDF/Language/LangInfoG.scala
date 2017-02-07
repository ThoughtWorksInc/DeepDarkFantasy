package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.RecursiveInfo

trait LangInfoG[X] extends RecursiveInfo[LangInfoGMatch, X] {
  def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[X]
}
