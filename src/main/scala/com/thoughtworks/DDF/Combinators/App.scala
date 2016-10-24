package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arrow

trait App[Info[_], Repr[_]] extends Arrow[Info, Repr] {
  def App[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => B) => A => B]

  final def App_[A, B]: Repr[A => B] => Repr[A => B] = f =>
    app(App(arrowDomainInfo(reprInfo(f)), arrowRangeInfo(reprInfo(f))))(f)

  final def App__[A, B]: Repr[A => B] => Repr[A] => Repr[B] = f => app(App_(f))
}
