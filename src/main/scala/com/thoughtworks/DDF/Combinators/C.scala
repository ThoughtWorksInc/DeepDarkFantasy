package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait C[Info[_], Repr[_]] extends Arr[Info, Repr] {
  def C[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(A => B => C) => (B => A => C)]

  final def C_[A, B, C]: Repr[A => B => C] => Repr[B => A => C] = f =>
    app(C[A, B, C](
      domInfo(reprInfo(f)),
      domInfo(rngInfo(reprInfo(f))),
      rngInfo(rngInfo(reprInfo(f)))))(f)

  final def C__[A, B, C]: Repr[A => B => C] => Repr[B] => Repr[A => C] = f => app(C_(f))
}
