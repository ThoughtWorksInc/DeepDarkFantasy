package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arr.ArrLang

trait CLang[Info[_], Repr[_]] extends ArrLang[Info, Repr] {
  def C[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(A => B => C) => (B => A => C)]
}
