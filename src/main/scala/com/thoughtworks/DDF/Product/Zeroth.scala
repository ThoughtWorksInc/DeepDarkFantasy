package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.Arr

trait Zeroth extends ProdType with Arr {
  def zro[A <: Type: Kind, B <: Type: Kind]: Prod[A, B] ~>: A

  final def zro_[A <: Type: Kind, B <: Type: Kind]: Prod[A, B] => A = app(zro[A, B])
}
