package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.ArrLang

trait BLang[Info[_], Repr[_]] extends ArrLang[Info, Repr] {
  def B[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(B => C) => (A => B) => (A => C)]
}
