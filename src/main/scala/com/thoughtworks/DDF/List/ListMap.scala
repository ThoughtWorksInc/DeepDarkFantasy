package com.thoughtworks.DDF.List

trait ListMap[Info[_], Repr[_]] extends ListMin[Info, Repr] {
  def listMap[A: Info, B: Info]: Repr[(A => B) => scala.List[A] => scala.List[B]]

  final def listMap_[A: Info, B: Info](f: Repr[A => B]): Repr[scala.List[A] => scala.List[B]] = app(listMap[A, B])(f)

  final def listMap__[A: Info, B: Info](f: Repr[A => B])(l: Repr[scala.List[A]]): Repr[scala.List[B]] =
    app(listMap_(f))(l)
}
