package com.thoughtworks.DDF

trait WLang[Info[_], Repr[_]] extends ArrLang[Info, Repr] {
  def W[A, B]: Repr[(A => A => B) => (A => B)]
}
