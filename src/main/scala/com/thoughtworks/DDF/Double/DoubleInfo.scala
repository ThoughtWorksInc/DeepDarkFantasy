package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ArrInfo

trait DoubleInfo[Info[_], Repr[_]] extends ArrInfo[Info, Repr] {
  implicit def doubleInfo: Info[scala.Double]
}
