package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Product.Product

trait FoldLeft[Info[_], Repr[_]] extends Product[Info, Repr] with ListMin[Info, Repr] {
  def foldLeft[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => B => A) => A => scala.List[B] => A]

  final def foldLeft_[A, B]: Repr[A => B => A] => Repr[A => scala.List[B] => A] = f =>
    app(foldLeft(domInfo(reprInfo(f)), domInfo(rngInfo(reprInfo(f)))))(f)

  final def foldLeft__[A, B]: Repr[A => B => A] => Repr[A] => Repr[scala.List[B] => A] = f => app(foldLeft_(f))

  final def foldLeft___[A, B]: Repr[A => B => A] => Repr[A] => Repr[scala.List[B]] => Repr[A] = f => x =>
    app(foldLeft__(f)(x))
}
