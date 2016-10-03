package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait WRepr[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  def W[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => A => B) => (A => B)]
}
