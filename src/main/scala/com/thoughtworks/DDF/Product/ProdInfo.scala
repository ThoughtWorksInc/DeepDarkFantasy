package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.ArrInfo

trait ProdInfo[Info[_], Repr[_]] extends ArrInfo[Info, Repr] {
  implicit def prodInfo[A : Info, B : Info]: Info[(A, B)]
}
