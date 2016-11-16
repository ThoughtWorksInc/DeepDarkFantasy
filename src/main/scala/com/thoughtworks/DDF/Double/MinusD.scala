package com.thoughtworks.DDF.Double

trait MinusD[Info[_], Repr[_]] extends LitD[Info, Repr] {
  def minusD: Repr[scala.Double => scala.Double => scala.Double]

  final val minusD_ : Repr[scala.Double] => Repr[scala.Double => scala.Double] = app(minusD)

  final val minusD__ : Repr[scala.Double] => Repr[scala.Double] => Repr[scala.Double] = x => app(minusD_(x))
}
