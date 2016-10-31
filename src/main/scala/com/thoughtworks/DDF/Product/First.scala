package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.Arr

trait First[Info[_], Repr[_]] extends ProductInfo[Info, Repr] with Arr[Info, Repr] {
  def first[A, B](implicit ai: Info[A], bi: Info[B]): Repr[((A, B)) => B]

  final def first_[A, B]: Repr[(A, B)] => Repr[B] = x =>
    app(first(productZerothInfo(reprInfo(x)), productFirstInfo(reprInfo(x))))(x)
}
