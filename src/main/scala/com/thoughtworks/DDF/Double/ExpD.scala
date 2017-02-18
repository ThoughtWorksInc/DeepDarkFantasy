package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr

trait ExpD extends DoubleType with Arr {
  def expD: Double ~>: Double

  final def expD_ : Double => Double = app(expD)
}
