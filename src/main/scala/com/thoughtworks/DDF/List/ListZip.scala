package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Product.ProdMin

trait ListZip extends ProdMin with ListMin {
  def listZip[A <: Type: Kind, B <: Type: Kind]: List[A] ~>: List[B] ~>: List[Prod[A, B]]

  final def listZip_[A <: Type: Kind, B <: Type: Kind](la: List[A]) = app(listZip[A, B])(la)

  final def listZip__[A <: Type: Kind, B <: Type: Kind](la: List[A])(lb: List[B]) = app(listZip_[A, B](la))(lb)
}
