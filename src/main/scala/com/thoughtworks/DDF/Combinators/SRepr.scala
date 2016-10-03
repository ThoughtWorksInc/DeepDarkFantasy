package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait SRepr[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  def S[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(A => B => C) => (A => B) => A => C]
}
