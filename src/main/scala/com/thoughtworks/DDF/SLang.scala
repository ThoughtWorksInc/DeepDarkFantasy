package com.thoughtworks.DDF

trait SLang[Info[_], Repr[_]] extends ArrLang[Info, Repr] {
  def S[A, B, C](implicit at: Info[A], bt: Info[B], ct: Info[C]): Repr[(A => B => C) => (A => B) => A => C]
}
