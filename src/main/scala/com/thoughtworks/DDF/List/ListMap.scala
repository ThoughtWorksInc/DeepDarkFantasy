package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Product.Prod

trait ListMap[Info[_], Repr[_]] extends Prod[Info, Repr] with ListMin[Info, Repr] {
  def listMap[A, B](implicit ai: Info[A], bi: Info[B]): Repr[(A => B) => scala.List[A] => scala.List[B]]

  final def listMap_[A, B]: Repr[A => B] => Repr[scala.List[A] => scala.List[B]] = f =>
    app(listMap(domInfo(reprInfo(f)), rngInfo(reprInfo(f))))(f)

  final def listMap__[A, B]: Repr[A => B] => Repr[scala.List[A]] => Repr[scala.List[B]] = f => app(listMap_(f))
}
