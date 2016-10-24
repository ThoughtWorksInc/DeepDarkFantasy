package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arrow

trait LitD[Info[_], Repr[_]] extends DoubleInfo[Info, Repr] with Arrow[Info, Repr] {
  def litD: scala.Double => Repr[scala.Double]
}
