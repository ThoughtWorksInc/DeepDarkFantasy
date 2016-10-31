package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr

trait LitD[Info[_], Repr[_]] extends DoubleInfo[Info, Repr] with Arr[Info, Repr] {
  def litD: scala.Double => Repr[scala.Double]
}
