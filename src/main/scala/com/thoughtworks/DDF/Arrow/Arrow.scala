package com.thoughtworks.DDF.Arrow

trait Arrow[Info[_], Repr[_]] extends ArrowMin[Info, Repr] {
  def let[A, B]: Repr[A] => Repr[A => B] => Repr[B] = x => y => app(y)(x)
}
