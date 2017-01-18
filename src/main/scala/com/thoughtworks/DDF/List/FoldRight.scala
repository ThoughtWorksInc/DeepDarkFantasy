package com.thoughtworks.DDF.List

trait FoldRight[Info[_], Repr[_]] extends ListMin[Info, Repr] {
  def foldRight[A: Info, B: Info]: Repr[(A => B => B) => B => scala.List[A] => B]

  final def foldRight_[A: Info, B: Info](f: Repr[A => B => B]): Repr[B => scala.List[A] => B] = app(foldRight[A, B])(f)

  final def foldRight__[A: Info, B: Info](f: Repr[A => B => B])(b: Repr[B]): Repr[scala.List[A] => B] =
    app(foldRight_(f))(b)

  final def foldRight___[A: Info, B: Info](f: Repr[A => B => B])(x: Repr[B])(l: Repr[scala.List[A]]): Repr[B] =
    app(foldRight__(f)(x))(l)
}
