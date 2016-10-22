package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait K[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  def K[A, B](implicit ai: Info[A], bi: Info[B]): Repr[A => B => A]

  final def K_[A, B](a: Repr[A])(implicit bi: Info[B]): Repr[B => A] =
    app(K[A, B](reprInfo(a), bi))(a)

  final def K__[A, B]: Repr[A] => Repr[B] => Repr[A] = a => b => app(K_[A, B](a)(reprInfo(b)))(b)
}
