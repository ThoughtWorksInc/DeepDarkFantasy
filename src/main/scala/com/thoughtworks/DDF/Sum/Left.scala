package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.Arr

trait Left[Info[_], Repr[_]] extends Arr[Info, Repr] with SumInfo[Info, Repr] {
  def left[A: Info, B: Info]: Repr[A => Either[A, B]]

  final def left_[A: Info, B: Info](a: Repr[A]): Repr[Either[A, B]] = app(left[A, B])(a)
}
