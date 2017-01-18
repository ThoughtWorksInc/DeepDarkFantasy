package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.ArrInfo

trait ProdInfo[Info[_], Repr[_]] extends ArrInfo[Info, Repr] {
  implicit def prodInfo[A, B](implicit ai: Info[A], bi: Info[B]): Info[(A, B)]

  def prodZroInfo[A, B]: Info[(A, B)] => Info[A]

  def prodFstInfo[A, B]: Info[(A, B)] => Info[B]
}
