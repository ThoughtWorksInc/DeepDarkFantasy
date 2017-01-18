package com.thoughtworks.DDF.List

trait FoldRight[Info[_], Repr[_]] extends ListMin[Info, Repr] {
  def foldRight[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => B => B) => B => scala.List[A] => B]

  final def foldRight_[A, B]: Repr[A => B => B] => Repr[B => scala.List[A] => B] = f =>
    app(foldRight(domInfo(reprInfo(f)), rngInfo(rngInfo(reprInfo(f)))))(f)

  final def foldRight__[A, B]: Repr[A => B => B] => Repr[B] => Repr[scala.List[A] => B] = f => app(foldRight_(f))

  final def foldRight___[A, B]: Repr[A => B => B] => Repr[B] => Repr[scala.List[A]] => Repr[B] = f => x =>
    app(foldRight__(f)(x))
}
