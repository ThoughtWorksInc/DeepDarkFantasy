package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr

trait PlusD extends DoubleType with Arr {
  def plusD: Double ~>: Double ~>: Double

  final def plusD_ : Double => Double ~>: Double = app(plusD)

  final def plusD__ : Double => Double => Double = l => app(plusD_(l))
}
