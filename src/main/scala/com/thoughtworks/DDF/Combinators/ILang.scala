package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arr.ArrLang

trait ILang[Info[_], Repr[_]] extends ArrLang[Info, Repr] {
  def I[A](implicit at: Info[A]): Repr[A => A]
}
