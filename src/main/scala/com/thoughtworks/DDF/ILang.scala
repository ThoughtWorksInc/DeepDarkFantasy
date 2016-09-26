package com.thoughtworks.DDF

trait ILang[Info[_], Repr[_]] extends ArrLang[Info, Repr] {
  def I[A](implicit at: Info[A]): Repr[A => A]
}
