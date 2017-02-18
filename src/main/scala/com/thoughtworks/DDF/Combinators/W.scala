package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait W extends Arr {
  def W[A <: Type: Kind, B <: Type: Kind]: (A ~>: A ~>: B) ~>: (A ~>: B)

  final def W_[A <: Type: Kind, B <: Type: Kind](f: A ~>: A ~>: B) = app(W[A, B])(f)

  final def W__[A <: Type: Kind, B <: Type: Kind](f: A ~>: A ~>: B)(a: A) = app(W_(f))(a)
}
