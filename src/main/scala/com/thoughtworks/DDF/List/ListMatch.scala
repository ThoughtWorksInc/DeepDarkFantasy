package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.ArrMin

trait ListMatch[Info[_], Repr[_]] extends ListInfo[Info, Repr] with ArrMin[Info, Repr] {
  def listMatch[A, B](implicit ai: Info[A], bi: Info[B]): Repr[scala.List[A] => B => (A => scala.List[A] => B) => B]

  final def listMatch_[A, B](l: Repr[scala.List[A]])(implicit bi: Info[B]): Repr[B => (A => scala.List[A] => B) => B] =
    app(listMatch[A, B](listElmInfo(reprInfo(l)), bi))(l)

  final def listMatch__[A, B]: Repr[scala.List[A]] => Repr[B] => Repr[(A => scala.List[A] => B) => B] = l => z =>
    app(listMatch_[A, B](l)(reprInfo(z)))(z)

  final def listMatch___[A, B]: Repr[scala.List[A]] => Repr[B] => Repr[A => scala.List[A] => B] => Repr[B] = l => z =>
    app(listMatch__(l)(z))
}
