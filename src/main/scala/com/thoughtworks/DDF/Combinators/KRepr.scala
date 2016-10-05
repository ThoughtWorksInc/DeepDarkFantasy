package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait KRepr[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  def K[A, B](implicit ai: Info[A], bi: Info[B]): Repr[A => B => A]
}
