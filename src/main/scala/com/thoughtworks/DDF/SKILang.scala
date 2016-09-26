package com.thoughtworks.DDF

trait SKILang[Info[_], Repr[_]] extends ArrLang[Info, Repr] {
  def S[A, B, C](implicit at: Info[A], bt: Info[B], ct: Info[C]): Repr[(A => B => C) => (A => B) => A => C]

  def K[A, B](implicit at: Info[A], bt: Info[B]): Repr[A => B => A]

  def I[A](implicit at: Info[A]): Repr[A => A]
}
