package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait C[Info[_], Repr[_]] extends Arr[Info, Repr] {
  def C[A: Info, B: Info, C: Info]: Repr[(A => B => C) => (B => A => C)]

  final def C_[A: Info, B: Info, C: Info](f: Repr[A => B => C]): Repr[B => A => C] = app(C[A, B, C])(f)

  final def C__[A: Info, B: Info, C: Info](f: Repr[A => B => C])(b: Repr[B]): Repr[A => C] = app(C_(f))(b)

  final def C___[A: Info, B: Info, C: Info](f: Repr[A => B => C])(b: Repr[B])(a: Repr[A]): Repr[C] = app(C__(f)(b))(a)
}
