package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr
import com.thoughtworks.DDF.Bool.BoolType

trait LtD extends DoubleType with BoolType with Arr {
  def ltD: Double ~>: Double ~>: Boolean

  final def ltD_ : Double => Double ~>: Boolean = app(ltD)

  final def ltD__ : Double => Double => Boolean = l => app(ltD_(l))
}
