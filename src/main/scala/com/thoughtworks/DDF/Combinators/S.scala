package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arrow

trait S[Info[_], Repr[_]] extends Arrow[Info, Repr] {
  def S[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(A => B => C) => (A => B) => A => C]

  final def S_[A, B, C]: Repr[A => B => C] => Repr[(A => B) => A => C] = f =>
    app(S[A, B, C](
      arrowDomainInfo(reprInfo(f)),
      arrowDomainInfo(arrowRangeInfo(reprInfo(f))),
      arrowRangeInfo(arrowRangeInfo(reprInfo(f)))))(f)

  final def S__[A, B, C]: Repr[A => B => C] => Repr[A => B] => Repr [A => C] = f => x => app(S_(f))(x)

  final def S___[A, B, C]: Repr[A => B => C] => Repr[A => B] => Repr[A] => Repr[C] = f => x => a => app(S__(f)(x))(a)
}
