package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr

trait NegD[Info[_], Repr[_]] extends LitD[Info, Repr] with Arr[Info, Repr] {
  def negD: Repr[scala.Double => scala.Double]

  final val negD_ : Repr[scala.Double] => Repr[scala.Double] = app(negD)
}
