package com.thoughtworks.DDF.List

trait Reverse[Info[_], Repr[_]] extends ListMin[Info, Repr] {
  def reverse[A: Info]: Repr[scala.List[A] => scala.List[A]]

  final def reverse_[A: Info](l: Repr[scala.List[A]]) = app(reverse)(l)
}
