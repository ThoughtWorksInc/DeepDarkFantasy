package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait ProductRepr[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  implicit def productInfo[A, B](implicit ai: Info[A], bi: Info[B]): Info[(A, B)]

  def productFirstInfo[A, B]: Info[(A, B)] => Info[A]

  def productSecondInfo[A, B]: Info[(A, B)] => Info[B]

  def mkProduct[A, B](implicit at: Info[A], bt: Info[B]): Repr[A => B => (A, B)]

  def zeroth[A, B](implicit at: Info[A], bt: Info[B]): Repr[((A, B)) => A]

  def first[A, B](implicit at: Info[A], bt: Info[B]): Repr[((A, B)) => B]

  def curry[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(((A, B)) => C) => A => B => C]

  def uncurry[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(A => B => C) => ((A, B)) => C]
}
