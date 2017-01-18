package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait K[Info[_], Repr[_]] extends Arr[Info, Repr] {
  def K[A: Info, B: Info]: Repr[A => B => A]

  final def K_[A: Info, B: Info](a: Repr[A]): Repr[B => A] = app(K[A, B])(a)

  final def K__[A: Info, B: Info](a: Repr[A])(b: Repr[B]): Repr[A] = app(K_[A, B](a))(b)
}
