package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.Arr

trait Right[Info[_], Repr[_]] extends Arr[Info, Repr] with SumInfo[Info, Repr] {
  def right[A, B](implicit ai: Info[A], bi: Info[B]): Repr[B => Either[A, B]]

  final def right_[A, B](b: Repr[B])(implicit ai: Info[A]): Repr[Either[A, B]] =
    app(right(ai, reprInfo(b)))(b)
}
