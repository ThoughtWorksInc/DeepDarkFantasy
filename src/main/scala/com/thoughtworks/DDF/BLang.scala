package com.thoughtworks.DDF

trait BLang[Info[_], Repr[_]] extends ArrLang[Info, Repr] {
  def B[A, B, C] : Repr[(B => C) => (A => B) => (A => C)]
}
