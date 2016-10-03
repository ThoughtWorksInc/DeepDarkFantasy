package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ArrowRepr

trait DoubleRepr[Info[_], Repr[_]] extends ArrowRepr[Info, Repr] {
  implicit def DoubleInfo: Info[Double]

  def LitD: Double => Repr[Double]

  def PlusD: Repr[Double => Double => Double]

  def MultD: Repr[Double => Double => Double]
}
