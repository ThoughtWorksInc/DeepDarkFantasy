package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.Arr

trait MkProd extends ProdType with Arr {
  def mkProd[A <: Type: Kind, B <: Type: Kind]: A ~>: B ~>: Prod[A, B]

  final def mkProd_[A <: Type: Kind, B <: Type: Kind](a: A): B ~>: Prod[A, B] = app(mkProd[A, B])(a)

  final def mkProd__[A, B]: Repr[A] => Repr[B] => Repr[(A, B)] = a => b => app(mkProd_(a)(reprInfo(b)))(b)
}
