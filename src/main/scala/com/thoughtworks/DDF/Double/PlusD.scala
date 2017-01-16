package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr

trait PlusD[Info[_], Repr[_]] extends LitD[Info, Repr] with Arr[Info, Repr] {
  def plusD: Repr[scala.Double => scala.Double => scala.Double]

  final def plusD_ : Repr[scala.Double] => Repr[scala.Double => scala.Double] = app(plusD)

  final def plusD__ : Repr[scala.Double] => Repr[scala.Double] => Repr[scala.Double] = l => app(plusD_(l))
}
