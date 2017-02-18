package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait K extends Arr {
  def K[A <: Type: Kind, B <: Type: Kind]: A ~>: B ~>: A

  final def K_[A <: Type: Kind, B <: Type: Kind](a: A) = app(K[A, B])(a)

  final def K__[A <: Type: Kind, B <: Type: Kind](a: A)(b: B) = app(K_[A, B](a))(b)
}
