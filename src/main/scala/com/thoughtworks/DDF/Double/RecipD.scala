package com.thoughtworks.DDF.Double

trait RecipD[Info[_], Repr[_]] extends LitD[Info, Repr] {
  def recipD: Repr[scala.Double => scala.Double]

  final val recipD_ : Repr[scala.Double] => Repr[scala.Double] = app(recipD)
}
