package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Product.Product

trait ListZip[Info[_], Repr[_]] extends Product[Info, Repr] with ListMin[Info, Repr] {
  def listZip[A, B](implicit ai: Info[A], bi: Info[B]): Repr[scala.List[A] => scala.List[B] => scala.List[(A, B)]]

  final def listZip_[A, B](la: Repr[scala.List[A]])(implicit bi: Info[B]): Repr[scala.List[B] => scala.List[(A, B)]] =
    app(listZip(listElmInfo(reprInfo(la)), bi))(la)

  final def listZip__[A, B]: Repr[scala.List[A]] => Repr[scala.List[B]] => Repr[scala.List[(A, B)]] = la => lb =>
    app(listZip_(la)(listElmInfo(reprInfo(lb))))(lb)
}
