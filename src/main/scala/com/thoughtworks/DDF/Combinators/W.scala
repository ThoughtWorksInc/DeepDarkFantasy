package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arrow

trait W[Info[_], Repr[_]] extends Arrow[Info, Repr] {
  def W[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => A => B) => (A => B)]

  final def W_[A, B]: Repr[A => A => B] => Repr[A => B] = f =>
    app(W[A, B](arrowDomainInfo(reprInfo(f)), arrowRangeInfo(arrowRangeInfo(reprInfo(f)))))(f)

  final def W__[A, B]: Repr[A => A => B] => Repr[A] => Repr[B] = f => app(W_(f))
}
