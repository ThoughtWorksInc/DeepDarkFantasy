package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr

trait SigD extends LitD with Arr {
  def sigD: Double ~>: Double

  final def sigD_ : Double => Double = app(sigD)
}
