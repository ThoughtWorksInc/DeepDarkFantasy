package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.ArrInfo

trait SumInfo[Info[_], Repr[_]] extends ArrInfo[Info, Repr] {
  implicit def sumInfo[A : Info, B : Info]: Info[Either[A, B]]
}
