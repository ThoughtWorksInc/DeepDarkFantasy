package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr

trait DivD[Info[_], Repr[_]] extends LitD[Info, Repr] with Arr[Info, Repr] {
  def divD: Repr[scala.Double => scala.Double => scala.Double]

  final lazy val divD_ : Repr[scala.Double] => Repr[scala.Double => scala.Double] = app(divD)

  final lazy val divD__ : Repr[scala.Double] => Repr[scala.Double] => Repr[scala.Double] = l => app(divD_(l))
}
