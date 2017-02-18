package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.Arr

trait I extends Arr {
  def I[A <: Type: Kind]: A ~>: A

  final def I_[A <: Type: Kind](a: A) = app(I[A])(a)
}
