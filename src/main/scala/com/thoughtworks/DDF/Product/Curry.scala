package com.thoughtworks.DDF.Product

trait Curry[Info[_], Repr[_]] extends ProdMin[Info, Repr] {
  def curry[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(((A, B)) => C) => A => B => C]

  final def curry_[A, B, C]: Repr[((A, B)) => C] => Repr[A => B => C] = f =>
    app(curry(
      prodZroInfo(domInfo(reprInfo(f))),
      prodFstInfo(domInfo(reprInfo(f))),
      rngInfo(reprInfo(f))))(f)

  final def curry__[A, B, C]: Repr[((A, B)) => C] => Repr[A] => Repr[B => C] = f => app(curry_(f))

  final def curry___[A, B, C]: Repr[((A, B)) => C] => Repr[A] => Repr[B] => Repr[C] = f => a => app(curry__(f)(a))
}
