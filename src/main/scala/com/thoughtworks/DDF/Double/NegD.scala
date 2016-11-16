package com.thoughtworks.DDF.Double

trait NegD[Info[_], Repr[_]] extends LitD[Info, Repr] {
  def negD: Repr[scala.Double => scala.Double]

  final val negD_ : Repr[scala.Double] => Repr[scala.Double] = app(negD)
}
