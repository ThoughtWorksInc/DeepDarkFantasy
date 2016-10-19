package com.thoughtworks.DDF.Arrow

trait ArrowRepr[Info[_], Repr[_]] extends ArrowMin[Info, Repr] {
  def let[A, B]: Repr[A] => Repr[A => B] => Repr[B] = x => y => app(y)(x)
}
