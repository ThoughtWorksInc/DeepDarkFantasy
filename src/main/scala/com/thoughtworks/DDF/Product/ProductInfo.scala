package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.ArrInfo

trait ProductInfo[Info[_], Repr[_]] extends ArrInfo[Info, Repr] {
  implicit def productInfo[A, B](implicit ai: Info[A], bi: Info[B]): Info[(A, B)]

  def productZerothInfo[A, B]: Info[(A, B)] => Info[A]

  def productFirstInfo[A, B]: Info[(A, B)] => Info[B]
}
