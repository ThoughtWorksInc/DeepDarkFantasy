package com.thoughtworks.DDF.Arrow

trait ArrowMin[Info[_], Repr[_]] extends ArrowInfo[Info, Repr] {
  def app[A, B]: Repr[A => B] => Repr[A] => Repr[B]
}
