package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait CRepr[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  def C[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(A => B => C) => (B => A => C)]
}
