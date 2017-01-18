package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait Z[Info[_], Repr[_]] extends Arr[Info, Repr] {
  def Z[A : Info, B : Info]: Repr[((A => B) => (A => B)) => (A => B)]

  def Z_[A : Info, B : Info](f : Repr[(A => B) => (A => B)]) : Repr[A => B] = app(Z[A, B])(f)

  def Z__[A : Info, B : Info](f : Repr[(A => B) => (A => B)])(x : Repr[A]) : Repr[B] = app(Z_[A, B](f))(x)
}
