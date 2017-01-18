package com.thoughtworks.DDF.List

trait ScanLeft[Info[_], Repr[_]] extends ListMin[Info, Repr] {
  def scanLeft[A: Info, B: Info]: Repr[(B => A => B) => B => scala.List[A] => scala.List[B]]

  final def scanLeft_[A: Info, B: Info](f: Repr[B => A => B]): Repr[B => scala.List[A] => scala.List[B]] =
    app(scanLeft[A, B])(f)

  final def scanLeft__[A: Info, B: Info](f: Repr[B => A => B])(x: Repr[B]): Repr[scala.List[A] => scala.List[B]] =
    app(scanLeft_(f))(x)

  final def scanLeft___[A: Info, B: Info](f: Repr[B => A => B])(x: Repr[B])(l: Repr[scala.List[A]]):
  Repr[scala.List[B]] = app(scanLeft__(f)(x))(l)
}
