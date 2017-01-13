package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr

trait SigD[Info[_], Repr[_]] extends LitD[Info, Repr] with Arr[Info, Repr] {
  def sigD: Repr[scala.Double => scala.Double]

  final lazy val sigD_ : Repr[scala.Double] => Repr[scala.Double] = app(sigD)
}
