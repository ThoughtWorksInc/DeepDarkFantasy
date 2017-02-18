package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr

trait MultD extends DoubleType with Arr {
  def multD: Double ~>: Double ~>: Double

  final def multD_ : Double => Double ~>: Double = app(multD)

  final def multD__ : Double => Double => Double = l => app(multD_(l))
}
