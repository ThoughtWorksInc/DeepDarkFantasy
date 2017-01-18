package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait I[Info[_], Repr[_]] extends Arr[Info, Repr] {
  def I[A: Info]: Repr[A => A]

  final def I_[A: Info](x: Repr[A]): Repr[A] = app(I[A])(x)
}
