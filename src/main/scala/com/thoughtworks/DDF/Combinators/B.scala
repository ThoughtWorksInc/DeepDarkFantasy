package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait B[Info[_], Repr[_]] extends Arr[Info, Repr] {
  def B[A : Info, B : Info, C : Info]: Repr[(B => C) => (A => B) => (A => C)]

  final def B_[A : Info, B : Info, C : Info](bc: Repr[B => C]): Repr[(A => B) => (A => C)] = app(B[A, B, C])(bc)

  final def B__[A : Info, B : Info, C : Info](bc : Repr[B => C])(ab : Repr[A => B]) : Repr[A => C] =
    app(B_[A, B, C](bc))(ab)

  final def B___[A : Info, B : Info, C : Info](bc : Repr[B => C])(ab : Repr[A => B])(a : Repr[A]) : Repr[C] =
    app(B__(bc)(ab))(a)
}
