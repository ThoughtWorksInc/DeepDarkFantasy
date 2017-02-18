package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.Arr

trait Left extends Arr with SumType {
  def left[A <: Type: Kind, B <: Type: Kind]: A ~>: Sum[A, B]

  final def left_[A <: Type: Kind, B <: Type: Kind](a: A): Sum[A, B] = app(left[A, B])(a)
}
