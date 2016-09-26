package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.ArrLang

trait CLang[Info[_], Repr[_]] extends ArrLang[Info, Repr] {
  def C[A, B, C] : Repr[(A => B => C) => (B => A => C)]
}
