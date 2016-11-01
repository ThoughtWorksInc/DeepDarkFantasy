package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.Arr

trait First[Info[_], Repr[_]] extends ProdInfo[Info, Repr] with Arr[Info, Repr] {
  def fst[A, B](implicit ai: Info[A], bi: Info[B]): Repr[((A, B)) => B]

  final def fst_[A, B]: Repr[(A, B)] => Repr[B] = x =>
    app(fst(prodZroInfo(reprInfo(x)), prodFstInfo(reprInfo(x))))(x)
}
