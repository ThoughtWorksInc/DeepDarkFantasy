package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait I[Info[_], Repr[_]] extends Arr[Info, Repr] {
  def I[A](implicit ai: Info[A]): Repr[A => A]

  final def I_[A]: Repr[A] => Repr[A] = a => app(I[A](reprInfo(a)))(a)
}
