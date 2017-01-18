package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.Arr

trait SumMatch[Info[_], Repr[_]] extends Arr[Info, Repr] with SumInfo[Info, Repr] {
  def sumMatch[A: Info, B: Info, C: Info]: Repr[Either[A, B] => (A => C) => (B => C) => C]

  final def sumMatch_[A: Info, B: Info, C: Info](e: Repr[Either[A, B]]): Repr[(A => C) => (B => C) => C] =
    app(sumMatch[A, B, C])(e)

  final def sumMatch__[A: Info, B: Info, C: Info](e: Repr[Either[A, B]])(ac: Repr[A => C]): Repr[(B => C) => C] =
    app(sumMatch_[A, B, C](e))(ac)

  final def sumMatch___[A: Info, B: Info, C: Info](e: Repr[Either[A, B]])(ac: Repr[A => C])(bc: Repr[B => C]): Repr[C] =
    app(sumMatch__(e)(ac))(bc)
}
