package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.InfoBase.InfoBase

trait ArrowInfo[Info[_], Repr[_]] extends InfoBase[Info, Repr] {
  implicit def aInfo[A, B](implicit ai : Info[A], bi : Info[B]) : Info[A => B]

  def domInfo[A, B]: Info[A => B] => Info[A]

  def rngInfo[A, B]: Info[A => B] => Info[B]
}
