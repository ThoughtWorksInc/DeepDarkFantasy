package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr

trait MinusD extends LitD with Arr {
  def minusD: Repr[scala.Double => scala.Double => scala.Double]

  final def minusD_ : Repr[scala.Double] => Repr[scala.Double => scala.Double] = app(minusD)

  final def minusD__ : Repr[scala.Double] => Repr[scala.Double] => Repr[scala.Double] = x => app(minusD_(x))
}
