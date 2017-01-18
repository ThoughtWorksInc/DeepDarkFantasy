package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait W[Info[_], Repr[_]] extends Arr[Info, Repr] {
  def W[A: Info, B: Info]: Repr[(A => A => B) => (A => B)]

  final def W_[A: Info, B: Info](f: Repr[A => A => B]): Repr[A => B] = app(W[A, B])(f)

  final def W__[A: Info, B: Info](f: Repr[A => A => B])(a: Repr[A]): Repr[B] = app(W_(f))(a)
}
