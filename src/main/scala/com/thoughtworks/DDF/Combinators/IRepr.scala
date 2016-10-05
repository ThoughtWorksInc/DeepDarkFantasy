package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait IRepr[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  def I[A](implicit ai: Info[A]): Repr[A => A]
}
