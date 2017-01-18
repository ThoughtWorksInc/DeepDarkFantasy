package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.Arr

trait ListMatch[Info[_], Repr[_]] extends ListInfo[Info, Repr] with Arr[Info, Repr] {
  def listMatch[A: Info, B: Info]: Repr[scala.List[A] => B => (A => scala.List[A] => B) => B]

  final def listMatch_[A: Info, B: Info](l: Repr[scala.List[A]]): Repr[B => (A => scala.List[A] => B) => B] =
    app(listMatch[A, B])(l)

  final def listMatch__[A: Info, B: Info](l: Repr[scala.List[A]])(z: Repr[B]): Repr[(A => scala.List[A] => B) => B] =
    app(listMatch_[A, B](l))(z)

  final def listMatch___[A: Info, B: Info](l: Repr[scala.List[A]])(z: Repr[B])(f: Repr[A => scala.List[A] => B]):
  Repr[B] = app(listMatch__(l)(z))(f)
}
