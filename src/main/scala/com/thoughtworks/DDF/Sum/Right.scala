package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.Arr

trait Right extends Arr with SumType {
  def right[A <: Type: Kind, B <: Type: Kind]: B ~>: Sum[A, B]

  final def right_[A <: Type: Kind, B <: Type: Kind](b: B): Sum[A, B] = app(right[A, B])(b)
}
