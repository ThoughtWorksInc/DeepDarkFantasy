package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arr.ArrLang

trait KLang[Info[_], Repr[_]] extends ArrLang[Info, Repr] {
  def K[A, B](implicit at: Info[A], bt: Info[B]): Repr[A => B => A]
}
