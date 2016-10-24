package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arrow

trait B[Info[_], Repr[_]] extends Arrow[Info, Repr] {
  def B[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(B => C) => (A => B) => (A => C)]

  final def B_[A, B, C](bc: Repr[B => C])(implicit ai: Info[A]): Repr[(A => B) => (A => C)] =
    app(B[A, B, C](ai, arrowDomainInfo(reprInfo(bc)), arrowRangeInfo(reprInfo(bc))))(bc)

  final def B__[A, B, C]: Repr[B => C] => Repr[A => B] => Repr[A => C] = bc => ab =>
    app(B_[A, B, C](bc)(arrowDomainInfo(reprInfo(ab))))(ab)

  final def B___[A, B, C]: Repr[B => C] => Repr[A => B] => Repr[A] => Repr[C] = bc => ab => a =>
    app(B__(bc)(ab))(a)
}
