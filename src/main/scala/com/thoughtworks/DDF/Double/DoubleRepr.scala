package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait DoubleRepr[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  implicit def doubleInfo: Info[Double]

  def litD: Double => Repr[Double]

  def plusD: Repr[Double => Double => Double]

  def multD: Repr[Double => Double => Double]
}
