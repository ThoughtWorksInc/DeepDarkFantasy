package com.thoughtworks.DDF.Product

trait UnCurry[Info[_], Repr[_]] extends ProductMin[Info, Repr] {
  def uncurry[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(A => B => C) => ((A, B)) => C]

  final def uncurry_[A, B, C]: Repr[A => B => C] => Repr[((A, B)) => C] = f =>
    app(uncurry(
      arrowDomainInfo(reprInfo(f)),
      arrowDomainInfo(arrowRangeInfo(reprInfo(f))),
      arrowRangeInfo(arrowRangeInfo(reprInfo(f)))))(f)

  final def uncurry__[A, B, C]: Repr[A => B => C] => Repr[(A, B)] => Repr[C] = f => app(uncurry_(f))
}
