package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr
import com.thoughtworks.DDF.Bool.BoolType

trait LtD extends DoubleType with BoolType with Arr {
  def ltD: Double ~>: Double ~>: Bool

  final def ltD_ : Double => Double ~>: Bool = app(ltD)_

  final def ltD__ : Double => Double => Bool = l => app(ltD_(l))_
}
