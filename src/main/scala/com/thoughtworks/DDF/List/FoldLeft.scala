package com.thoughtworks.DDF.List

trait FoldLeft[Info[_], Repr[_]] extends ListMin[Info, Repr] {
  def foldLeft[A: Info, B: Info]: Repr[(A => B => A) => A => scala.List[B] => A]

  final def foldLeft_[A: Info, B: Info](f: Repr[A => B => A]): Repr[A => scala.List[B] => A] = app(foldLeft[A, B])(f)

  final def foldLeft__[A: Info, B: Info](f: Repr[A => B => A])(a: Repr[A]): Repr[scala.List[B] => A] =
    app(foldLeft_(f))(a)

  final def foldLeft___[A: Info, B: Info](f: Repr[A => B => A])(a: Repr[A])(l: Repr[scala.List[B]]): Repr[A] =
    app(foldLeft__(f)(a))(l)
}
