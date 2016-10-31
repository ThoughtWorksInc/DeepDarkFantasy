package com.thoughtworks.DDF.Arrow

trait Arr[Info[_], Repr[_]] extends ArrMin[Info, Repr] {
  def let[A, B]: Repr[A] => Repr[A => B] => Repr[B] = x => y => app(y)(x)
}
