package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.Arr

trait Zeroth[Info[_], Repr[_]] extends ProdInfo[Info, Repr] with Arr[Info, Repr] {
  def zro[A, B](implicit ai: Info[A], bi: Info[B]): Repr[((A, B)) => A]

  final def zro_[A, B]: Repr[(A, B)] => Repr[A] = x =>
    app(zro(prodZroInfo(reprInfo(x)), prodFstInfo(reprInfo(x))))(x)
}
