package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait ProductRepr[Info[_], Repr[_]] extends ProductInfo[Info, Repr] with ArrowRepr[Info, Repr] {
  def mkProduct[A, B](implicit ai: Info[A], bi: Info[B]): Repr[A => B => (A, B)]

  def zeroth[A, B](implicit ai: Info[A], bi: Info[B]): Repr[((A, B)) => A]

  def first[A, B](implicit ai: Info[A], bi: Info[B]): Repr[((A, B)) => B]

  def curry[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(((A, B)) => C) => A => B => C]

  def uncurry[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(A => B => C) => ((A, B)) => C]
}
