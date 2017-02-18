package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait B extends Arr {
  def B[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind]: (B ~>: C) ~>: (A ~>: B) ~>: (A ~>: C)

  final def B_[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](bc: B ~>: C) = app(B[A, B, C])(bc)

  final def B__[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](bc: B ~>: C)(ab: A ~>: B) = app(B_[A, B, C](bc))(ab)

  final def B___[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](bc: B ~>: C)(ab: A ~>: B)(a: A) =
    app(B__(bc)(ab))(a)
}
