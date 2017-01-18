package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait App[Info[_], Repr[_]] extends Arr[Info, Repr] {
  def App[A: Info, B: Info]: Repr[(A => B) => A => B]

  final def App_[A: Info, B: Info](f: Repr[A => B]): Repr[A => B] = app(App[A, B])(f)

  final def App__[A: Info, B: Info](f: Repr[A => B])(x: Repr[A]): Repr[B] = app(App_[A, B](f))(x)
}
