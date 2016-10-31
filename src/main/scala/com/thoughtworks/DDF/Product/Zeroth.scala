package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.Arr

trait Zeroth[Info[_], Repr[_]] extends ProductInfo[Info, Repr] with Arr[Info, Repr] {
  def zeroth[A, B](implicit ai: Info[A], bi: Info[B]): Repr[((A, B)) => A]

  final def zeroth_[A, B]: Repr[(A, B)] => Repr[A] = x =>
    app(zeroth(productZerothInfo(reprInfo(x)), productFirstInfo(reprInfo(x))))(x)
}
