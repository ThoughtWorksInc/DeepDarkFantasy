package com.thoughtworks.DDF.String

import com.thoughtworks.DDF.Arrow.ArrInfo

trait StringInfo[Info[_], Repr[_]] extends ArrInfo[Info, Repr] {
  implicit def stringInfo: Info[scala.Predef.String]
}
