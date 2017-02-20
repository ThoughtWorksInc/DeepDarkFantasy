package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr

trait NegD extends LitD with Arr {
  def negD: Double ~>: Double

  final def negD_ = app(negD)_
}
