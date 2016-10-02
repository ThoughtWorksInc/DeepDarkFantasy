package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arr.ArrLang

trait DLang[Info[_], Repr[_]] extends ArrLang[Info, Repr] {
  implicit def DoubleInfo: Info[Double]

  def LitD: Double => Repr[Double]

  def PlusD: Repr[Double => Double => Double]

  def MultD: Repr[Double => Double => Double]
}
