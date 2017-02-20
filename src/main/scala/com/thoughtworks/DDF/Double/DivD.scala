package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.{Arr, ArrType}

trait DivD extends DoubleType with Arr {
  def divD: Double ~>: Double ~>: Double

  final def divD_ = app(divD)_

  final def divD__(l: Double) = app(divD_(l))_
}
