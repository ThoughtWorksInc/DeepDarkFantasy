package com.thoughtworks.DDF.Double

trait DivD[Info[_], Repr[_]] extends LitD[Info, Repr] {
  def divD: Repr[scala.Double => scala.Double => scala.Double]

  final def divD_ : Repr[scala.Double] => Repr[scala.Double => scala.Double] = app(divD)

  final def divD__ : Repr[scala.Double] => Repr[scala.Double] => Repr[scala.Double] = l => app(divD_(l))
}
