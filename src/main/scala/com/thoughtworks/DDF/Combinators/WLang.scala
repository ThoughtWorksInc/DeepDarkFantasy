package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arr.ArrLang

trait WLang[Info[_], Repr[_]] extends ArrLang[Info, Repr] {
  def W[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => A => B) => (A => B)]
}
