package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr

trait ExpD[Info[_], Repr[_]] extends LitD[Info, Repr] with Arr[Info, Repr] {
  def expD: Repr[scala.Double => scala.Double]

  final lazy val expD_ : Repr[scala.Double] => Repr[scala.Double] = app(expD)
}
