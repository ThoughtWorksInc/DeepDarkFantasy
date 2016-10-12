package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.InfoBase.InfoBase

trait ArrowInfo[Info[_], Repr[_]] extends InfoBase[Info, Repr] {
  implicit def arrowInfo[A, B](implicit ai : Info[A], bi : Info[B]) : Info[A => B]

  def arrowDomainInfo[A, B]: Info[A => B] => Info[A]

  def arrowRangeInfo[A, B]: Info[A => B] => Info[B]
}
