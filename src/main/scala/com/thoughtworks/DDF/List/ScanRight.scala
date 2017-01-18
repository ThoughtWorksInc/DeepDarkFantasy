package com.thoughtworks.DDF.List

trait ScanRight[Info[_], Repr[_]] extends ListMin[Info, Repr] {
  def scanRight[A: Info, B: Info]: Repr[(A => B => B) => B => scala.List[A] => scala.List[B]]

  final def scanRight_[A: Info, B: Info](f: Repr[A => B => B]): Repr[B => scala.List[A] => scala.List[B]] =
    app(scanRight[A, B])(f)

  final def scanRight__[A: Info, B: Info](f: Repr[A => B => B])(x: Repr[B]): Repr[scala.List[A] => scala.List[B]] =
    app(scanRight_(f))(x)

  final def scanRight___[A: Info, B: Info](f: Repr[A => B => B])(x: Repr[B])(l: Repr[scala.List[A]]):
  Repr[scala.List[B]] = app(scanRight__(f)(x))(l)
}
