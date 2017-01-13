package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.Arr

trait RecipD[Info[_], Repr[_]] extends LitD[Info, Repr] with Arr[Info, Repr] {
  def recipD: Repr[scala.Double => scala.Double]

  final lazy val recipD_ : Repr[scala.Double] => Repr[scala.Double] = app(recipD)
}
