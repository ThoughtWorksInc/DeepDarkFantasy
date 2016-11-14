package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.Arr

trait Cons[Info[_], Repr[_]] extends ListInfo[Info, Repr] with Arr[Info, Repr] {
  def cons[A](implicit ai: Info[A]): Repr[A => scala.List[A] => scala.List[A]]

  final def cons_[A]: Repr[A] => Repr[scala.List[A] => scala.List[A]] = a => app(cons(reprInfo(a)))(a)

  final def cons__[A]: Repr[A] => Repr[scala.List[A]] => Repr[scala.List[A]] = a => app(app(cons(reprInfo(a)))(a))
}
