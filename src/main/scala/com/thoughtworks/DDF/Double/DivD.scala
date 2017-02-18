package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr

trait DivD extends LitD with Arr {
  def divD: Repr[scala.Double => scala.Double => scala.Double]

  final def divD_ : Repr[scala.Double] => Repr[scala.Double => scala.Double] = app(divD)

  final def divD__ : Repr[scala.Double] => Repr[scala.Double] => Repr[scala.Double] = l => app(divD_(l))
}
