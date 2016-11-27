package com.thoughtworks.DDF.Hoas

trait Hoas[Info[_], Repr[_]] {
  def hoas[A, B]: (Repr[A] => Repr[B]) => Repr[A => B]
}
