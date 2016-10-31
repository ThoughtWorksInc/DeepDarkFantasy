package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait S[Info[_], Repr[_]] extends Arr[Info, Repr] {
  def S[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(A => B => C) => (A => B) => A => C]

  final def S_[A, B, C]: Repr[A => B => C] => Repr[(A => B) => A => C] = f =>
    app(S[A, B, C](
      domInfo(reprInfo(f)),
      domInfo(rngInfo(reprInfo(f))),
      rngInfo(rngInfo(reprInfo(f)))))(f)

  final def S__[A, B, C]: Repr[A => B => C] => Repr[A => B] => Repr [A => C] = f => app(S_(f))

  final def S___[A, B, C]: Repr[A => B => C] => Repr[A => B] => Repr[A] => Repr[C] = f => x => app(S__(f)(x))
}
