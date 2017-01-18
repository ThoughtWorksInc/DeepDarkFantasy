package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.Arr

trait First[Info[_], Repr[_]] extends ProdInfo[Info, Repr] with Arr[Info, Repr] {
  def fst[A: Info, B: Info]: Repr[((A, B)) => B]

  final def fst_[A: Info, B: Info](x: Repr[(A, B)]): Repr[B] = app(fst[A, B])(x)
}
