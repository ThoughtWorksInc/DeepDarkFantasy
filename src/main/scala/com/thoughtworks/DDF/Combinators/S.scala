package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait S[Info[_], Repr[_]] extends Arr[Info, Repr] {
  def S[A: Info, B: Info, C: Info]: Repr[(A => B => C) => (A => B) => A => C]

  final def S_[A: Info, B: Info, C: Info](f: Repr[A => B => C]): Repr[(A => B) => A => C] = app(S[A, B, C])(f)

  final def S__[A: Info, B: Info, C: Info](f: Repr[A => B => C])(x: Repr[A => B]): Repr [A => C] = app(S_(f))(x)

  final def S___[A: Info, B: Info, C: Info](f: Repr[A => B => C])(x: Repr[A => B])(a: Repr[A]): Repr[C] =
    app(S__(f)(x))(a)
}
