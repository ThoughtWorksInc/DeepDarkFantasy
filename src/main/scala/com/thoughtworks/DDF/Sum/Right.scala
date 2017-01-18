package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.Arr

trait Right[Info[_], Repr[_]] extends Arr[Info, Repr] with SumInfo[Info, Repr] {
  def right[A: Info, B: Info]: Repr[B => Either[A, B]]

  final def right_[A: Info, B: Info](b: Repr[B]): Repr[Either[A, B]] = app(right[A, B])(b)
}
