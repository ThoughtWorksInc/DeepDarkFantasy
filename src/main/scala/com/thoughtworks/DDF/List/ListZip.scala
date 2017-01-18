package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Product.ProdMin

trait ListZip[Info[_], Repr[_]] extends ProdMin[Info, Repr] with ListMin[Info, Repr] {
  def listZip[A: Info, B: Info]: Repr[scala.List[A] => scala.List[B] => scala.List[(A, B)]]

  final def listZip_[A: Info, B: Info](la: Repr[scala.List[A]]): Repr[scala.List[B] => scala.List[(A, B)]] =
    app(listZip[A, B])(la)

  final def listZip__[A: Info, B: Info](la: Repr[scala.List[A]])(lb: Repr[scala.List[B]]): Repr[scala.List[(A, B)]] =
    app(listZip_[A, B](la))(lb)
}
