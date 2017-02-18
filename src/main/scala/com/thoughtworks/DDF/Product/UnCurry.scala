package com.thoughtworks.DDF.Product

trait UnCurry extends ProdMin {
  def uncurry[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(A => B => C) => ((A, B)) => C]

  final def uncurry_[A, B, C]: Repr[A => B => C] => Repr[((A, B)) => C] = f =>
    app(uncurry(
      domInfo(reprInfo(f)),
      domInfo(rngInfo(reprInfo(f))),
      rngInfo(rngInfo(reprInfo(f)))))(f)

  final def uncurry__[A, B, C]: Repr[A => B => C] => Repr[(A, B)] => Repr[C] = f => app(uncurry_(f))
}
