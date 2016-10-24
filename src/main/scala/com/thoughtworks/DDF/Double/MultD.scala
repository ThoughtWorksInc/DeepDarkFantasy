package com.thoughtworks.DDF.Double

trait MultD[Info[_], Repr[_]] extends LitD[Info, Repr] {
  def multD: Repr[scala.Double => scala.Double => scala.Double]

  final def multD_ : Repr[scala.Double] => Repr[scala.Double => scala.Double] = app(multD)

  final def multD__ : Repr[scala.Double] => Repr[scala.Double] => Repr[scala.Double] = l => app(multD_(l))
}
