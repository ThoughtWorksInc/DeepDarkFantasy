package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait Let[Info[_], Repr[_]] extends Arr[Info, Repr] {
  def Let[A: Info, B: Info]: Repr[A => (A => B) => B]

  final def Let_[A: Info, B: Info](a: Repr[A]): Repr[(A => B) => B] = app(Let[A, B])(a)

  final def Let__[A: Info, B: Info](a: Repr[A])(f: Repr[(A => B)]): Repr[B] = app(Let_[A, B](a))(f)
}
