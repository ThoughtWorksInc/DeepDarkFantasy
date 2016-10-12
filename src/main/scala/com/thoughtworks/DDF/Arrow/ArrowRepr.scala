package com.thoughtworks.DDF.Arrow

trait ArrowRepr[Info[_], Repr[_]] extends ArrowInfo[Info, Repr] {
  def app[A, B]: Repr[A => B] => Repr[A] => Repr[B]

  def let[A, B]: Repr[A] => Repr[A => B] => Repr[B] = x => y => app(y)(x)
}
