package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.ArrowInfo

trait DoubleInfo[Info[_], Repr[_]] extends ArrowInfo[Info, Repr] {
  implicit def doubleInfo: Info[scala.Double]
}
