package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr

trait RecipD extends DoubleType with Arr {
  def recipD: Double ~>: Double

  final def recipD_ : Double => Double = app(recipD)
}
