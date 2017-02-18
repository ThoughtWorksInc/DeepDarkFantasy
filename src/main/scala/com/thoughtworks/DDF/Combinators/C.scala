package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait C extends Arr {
  def C[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind]: (A ~>: B ~>: C) ~>: (B ~>: A ~>: C)

  final def C_[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](f: A ~>: B ~>: C) = app(C[A, B, C])(f)

  final def C__[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](f: A ~>: B ~>: C)(b: B) = app(C_(f))(b)

  final def C___[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](f: A ~>: B ~>: C)(b: B)(a: A) = app(C__(f)(b))(a)
}
