package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arrow

trait Y[Info[_], Repr[_]] extends Arrow[Info, Repr] {
  def Y[A, B](implicit ai: Info[A], bi: Info[B]): Repr[((A => B) => (A => B)) => (A => B)]

  def Y_[A, B]: Repr[(A => B) => (A => B)] => Repr[A => B] = f =>
    app(Y(arrowDomainInfo(arrowDomainInfo(reprInfo(f))), arrowRangeInfo(arrowDomainInfo(reprInfo(f)))))(f)

  def Y__[A, B]: Repr[(A => B) => (A => B)] => Repr[A] => Repr[B] = f =>
    app(Y_(f))
}
