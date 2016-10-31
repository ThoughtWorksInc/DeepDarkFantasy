package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.Arr

trait SumMatch[Info[_], Repr[_]] extends Arr[Info, Repr] with SumInfo[Info, Repr] {
  def sumMatch[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[Either[A, B] => (A => C) => (B => C) => C]

  final def sumMatch_[A, B, C](e: Repr[Either[A, B]])(implicit ci: Info[C]): Repr[(A => C) => (B => C) => C] =
    app(sumMatch(sumLeftInfo(reprInfo(e)), sumRightInfo(reprInfo(e)), ci))(e)

  final def sumMatch__[A, B, C]: Repr[Either[A, B]] => Repr[A => C] => Repr[(B => C) => C] = e => lc =>
    app(sumMatch_(e)(rngInfo(reprInfo(lc))))(lc)

  final def sumMatch___[A, B, C]: Repr[Either[A, B]] => Repr[A => C] => Repr[B => C] => Repr[C] = e => lc =>
    app(sumMatch__(e)(lc))
}
