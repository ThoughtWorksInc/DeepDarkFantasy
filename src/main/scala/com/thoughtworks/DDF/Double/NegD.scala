package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr

trait NegD extends LitD with Arr {
  def negD: Repr[scala.Double => scala.Double]

  final def negD_ : Repr[scala.Double] => Repr[scala.Double] = app(negD)
}
