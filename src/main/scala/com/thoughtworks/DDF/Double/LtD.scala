package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Bool.Bool

trait LtD[Info[_], Repr[_]] extends LitD[Info, Repr] with Bool[Info, Repr] {
  def ltD: Repr[scala.Double => scala.Double => Boolean]

  final def ltD_ : Repr[scala.Double] => Repr[scala.Double => Boolean] = app(ltD)

  final def ltD__ : Repr[scala.Double] => Repr[scala.Double] => Repr[Boolean] = l => app(ltD_(l))
}
