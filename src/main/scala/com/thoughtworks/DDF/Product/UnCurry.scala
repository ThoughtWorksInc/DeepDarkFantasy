package com.thoughtworks.DDF.Product

trait UnCurry extends ProdMin {
  def uncurry[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind]: (A ~>: B ~>: C) ~>: Prod[A, B] ~>: C

  final def uncurry_[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](f: A ~>: B ~>: C) = app(uncurry[A, B, C])(f)

  final def uncurry__[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](f: A ~>: B ~>: C)(p: Prod[A, B]) =
    app(uncurry_(f))(p)
}
