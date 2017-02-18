package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait Let extends Arr {
  def Let[A <: Type: Kind, B <: Type: Kind]: A ~>: (A ~>: B) ~>: B

  final def Let_[A <: Type: Kind, B <: Type: Kind](a: A) = app(Let[A, B])(a)

  final def Let__[A <: Type: Kind, B <: Type: Kind](a: A)(f: A ~>: B) = app(Let_[A, B](a))(f)
}
