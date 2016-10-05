package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait App[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  def App[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => B) => A => B]
}
