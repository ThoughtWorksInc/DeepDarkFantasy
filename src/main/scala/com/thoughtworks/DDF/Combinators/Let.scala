package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait Let[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  def Let[A, B](implicit ai: Info[A], bi: Info[B]): Repr[A => (A => B) => B]
}
