package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr

trait MinusD extends DoubleType with Arr {
  def minusD: Double ~>: Double ~>: Double

  final def minusD_ = app(minusD)_

  final def minusD__(l: Double) = app(minusD_(l))_
}
