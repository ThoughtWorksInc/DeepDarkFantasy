package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait Y extends Arr {
  def Y[A <: Type: Kind, B <: Type: Kind]: ((A ~>: B) ~>: (A ~>: B)) ~>: (A ~>: B)

  final def Y_[A <: Type: Kind, B <: Type: Kind](f: (A ~>: B) ~>: (A ~>: B)) = app(Y[A, B])(f)

  final def Y__[A <: Type: Kind, B <: Type: Kind](f: (A ~>: B) ~>: (A ~>: B))(a: A) = app(Y_(f))(a)
}
