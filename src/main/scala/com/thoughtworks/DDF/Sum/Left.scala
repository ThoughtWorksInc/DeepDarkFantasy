package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.Arr

trait Left[Info[_], Repr[_]] extends Arr[Info, Repr] with SumInfo[Info, Repr] {
  def left[A, B](implicit ai: Info[A], bi: Info[B]): Repr[A => Either[A, B]]

  final def left_[A, B](a: Repr[A])(implicit bi: Info[B]): Repr[Either[A, B]] =
    app(left(reprInfo(a), bi))(a)
}
