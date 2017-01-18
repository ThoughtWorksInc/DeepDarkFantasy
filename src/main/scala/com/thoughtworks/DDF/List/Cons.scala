package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.Arr

trait Cons[Info[_], Repr[_]] extends ListInfo[Info, Repr] with Arr[Info, Repr] {
  def cons[A: Info]: Repr[A => scala.List[A] => scala.List[A]]

  final def cons_[A: Info](a: Repr[A]): Repr[scala.List[A] => scala.List[A]] = app(cons)(a)

  final def cons__[A: Info](a: Repr[A])(s: Repr[scala.List[A]]): Repr[scala.List[A]] = app(app(cons)(a))(s)
}
