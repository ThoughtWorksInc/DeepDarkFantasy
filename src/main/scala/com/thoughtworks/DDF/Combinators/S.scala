package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait S extends Arr {
  def S[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind]: (A ~>: B ~>: C) ~>: (A ~>: B) ~>: A ~>: C

  final def S_[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](f: A ~>: B ~>: C) = app(S[A, B, C])(f)

  final def S__[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](f: A ~>: B ~>: C)(x: A ~>: B) = app(S_(f))(x)

  final def S___[A <: Type: Kind, B <: Type: Kind, C <: Type: Kind](f: A ~>: B ~>: C)(x: A ~>: B)(a: A) =
    app(S__(f)(x))(a)
}
