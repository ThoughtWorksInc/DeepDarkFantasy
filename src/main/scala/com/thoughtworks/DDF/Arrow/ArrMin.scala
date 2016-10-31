package com.thoughtworks.DDF.Arrow

trait ArrMin[Info[_], Repr[_]] extends ArrInfo[Info, Repr] {
  def app[A, B]: Repr[A => B] => Repr[A] => Repr[B]
}
