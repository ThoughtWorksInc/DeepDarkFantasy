package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait BRepr[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  def B[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(B => C) => (A => B) => (A => C)]
}
