package com.thoughtworks.DDF.Double

trait ExpD[Info[_], Repr[_]] extends LitD[Info, Repr] {
  def expD: Repr[scala.Double => scala.Double]

  final def expD_ : Repr[scala.Double] => Repr[scala.Double] = app(expD)
}
