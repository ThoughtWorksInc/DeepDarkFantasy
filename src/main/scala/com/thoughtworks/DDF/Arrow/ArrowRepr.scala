package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.InfoB.InfoB

trait ArrowRepr[Info[_], Repr[_]] extends InfoB[Info, Repr] {
  implicit def ArrowInfo[A, B](implicit ai : Info[A], bi : Info[B]) : Info[A => B]

  def ArrDomInfo[A, B]: Info[A => B] => Info[A]

  def ArrRngInfo[A, B]: Info[A => B] => Info[B]

  def app[A, B]: Repr[A => B] => Repr[A] => Repr[B]
}
