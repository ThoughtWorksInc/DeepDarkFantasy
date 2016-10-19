package com.thoughtworks.DDF.Product

trait ProductRepr[Info[_], Repr[_]] extends ProductBasic[Info, Repr] {

  def curry[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(((A, B)) => C) => A => B => C]

  def uncurry[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(A => B => C) => ((A, B)) => C]
}
