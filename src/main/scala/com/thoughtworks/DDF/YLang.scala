package com.thoughtworks.DDF

trait YLang[Info[_], Repr[_]] extends ArrLang[Info, Repr] {
  def Y[A, B](implicit at: Info[A], bt: Info[B]): Repr[((A => B) => (A => B)) => (A => B)]
}
