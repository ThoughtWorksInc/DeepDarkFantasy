package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait Double[Info[_], Repr[_]] extends DoubleInfo[Info, Repr] with ArrowRepr[Info, Repr] {
  def litD: scala.Double => Repr[scala.Double]

  def plusD: Repr[scala.Double => scala.Double => scala.Double]

  def multD: Repr[scala.Double => scala.Double => scala.Double]

  def divD: Repr[scala.Double => scala.Double => scala.Double]

  def expD: Repr[scala.Double => scala.Double]

  def sigD: Repr[scala.Double => scala.Double]
}
