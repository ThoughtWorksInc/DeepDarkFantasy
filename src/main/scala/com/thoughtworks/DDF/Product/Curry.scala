package com.thoughtworks.DDF.Product

trait Curry extends ProdMin {
  def curry[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind]: (Prod[A, B] ~>: C) ~>: A ~>: B ~>: C

  final def curry_[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](f: Prod[A, B] ~>: C) = app(curry[A, B, C])(f)

  final def curry__[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](f: Prod[A, B] ~>: C)(a: A) = app(curry_(f))(a)

  final def curry___[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](f: Prod[A, B] ~>: C)(a: A)(b: B) =
    app(curry__(f)(a))(b)
}
