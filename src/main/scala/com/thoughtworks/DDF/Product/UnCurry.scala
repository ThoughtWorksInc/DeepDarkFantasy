package com.thoughtworks.DDF.Product

trait UnCurry[Info[_], Repr[_]] extends ProdMin[Info, Repr] {
  def uncurry[A: Info, B: Info, C: Info]: Repr[(A => B => C) => ((A, B)) => C]

  final def uncurry_[A: Info, B: Info, C: Info](f: Repr[A => B => C]): Repr[((A, B)) => C] =
    app(uncurry[A, B, C])(f)

  final def uncurry__[A: Info, B: Info, C: Info](f: Repr[A => B => C])(p: Repr[(A, B)]): Repr[C] = app(uncurry_(f))(p)
}
