package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait DoubleRepr[Info[_], Repr[_]] extends DoubleInfo[Info, Repr] with ArrowRepr[Info, Repr] {
  def litD: Double => Repr[Double]

  def plusD: Repr[Double => Double => Double]

  def multD: Repr[Double => Double => Double]

  def divD: Repr[Double => Double => Double]

  def expD: Repr[Double => Double]
}
