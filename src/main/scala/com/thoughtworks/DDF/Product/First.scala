package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.Arr

trait First extends ProdType with Arr {
  def fst[A <: Type: Kind, B <: Type: Kind]: Prod[A, B] ~>: B

  final def fst_[A <: Type: Kind, B <: Type: Kind](p: Prod[A, B]): B = app(fst[A, B])(p)
}
