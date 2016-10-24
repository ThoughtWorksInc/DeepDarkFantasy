package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.Arrow

trait ProductMin[Info[_], Repr[_]] extends ProductInfo[Info, Repr] with Arrow[Info, Repr] {
  def mkProduct[A, B](implicit ai: Info[A], bi: Info[B]): Repr[A => B => (A, B)]

  def zeroth[A, B](implicit ai: Info[A], bi: Info[B]): Repr[((A, B)) => A]

  def first[A, B](implicit ai: Info[A], bi: Info[B]): Repr[((A, B)) => B]
}
