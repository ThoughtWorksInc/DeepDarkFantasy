package com.thoughtworks.DDF.Arrow

trait Arr[Info[_], Repr[_]] extends ArrInfo[Info, Repr] {
  def app[A: Info, B: Info](f: Repr[A => B])(x: Repr[A]): Repr[B]
}
