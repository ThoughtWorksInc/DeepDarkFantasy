package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.Arr

trait Zeroth[Info[_], Repr[_]] extends ProdInfo[Info, Repr] with Arr[Info, Repr] {
  def zro[A: Info, B: Info]: Repr[((A, B)) => A]

  final def zro_[A: Info, B: Info](x: Repr[(A, B)]): Repr[A] = app(zro[A, B])(x)
}
