package com.thoughtworks.DDF.Product

trait Curry[Info[_], Repr[_]] extends ProdMin[Info, Repr] {
  def curry[A: Info, B: Info, C: Info]: Repr[(((A, B)) => C) => A => B => C]

  final def curry_[A: Info, B: Info, C: Info](f: Repr[((A, B)) => C]): Repr[A => B => C] =
    app(curry[A, B, C])(f)

  final def curry__[A: Info, B: Info, C: Info](f: Repr[((A, B)) => C])(a: Repr[A]): Repr[B => C] = app(curry_(f))(a)

  final def curry___[A: Info, B: Info, C: Info](f: Repr[((A, B)) => C])(a: Repr[A])(b: Repr[B]): Repr[C] =
    app(curry__(f)(a))(b)
}
