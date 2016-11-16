package com.thoughtworks.DDF.Double

trait LitD[Info[_], Repr[_]] extends DoubleInfo[Info, Repr] {
  def litD: scala.Double => Repr[scala.Double]
}
