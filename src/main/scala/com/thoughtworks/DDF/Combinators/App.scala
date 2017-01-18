package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait App[Info[_], Repr[_]] extends Arr[Info, Repr] {
  def App[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => B) => A => B]

  final def App_[A, B]: Repr[A => B] => Repr[A => B] = f =>
    app(App(domInfo(reprInfo(f)), rngInfo(reprInfo(f))))(f)

  final def App__[A, B]: Repr[A => B] => Repr[A] => Repr[B] = f => app(App_(f))
}
