package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.Arr

trait MkProd[Info[_], Repr[_]] extends ProdInfo[Info, Repr] with Arr[Info, Repr] {
  def mkProd[A: Info, B: Info]: Repr[A => B => (A, B)]

  final def mkProd_[A: Info, B: Info](a: Repr[A]): Repr[B => (A, B)] = app(mkProd[A, B])(a)

  final def mkProd__[A: Info, B: Info](a: Repr[A])(b: Repr[B]): Repr[(A, B)] = app(mkProd_[A, B](a))(b)
}
