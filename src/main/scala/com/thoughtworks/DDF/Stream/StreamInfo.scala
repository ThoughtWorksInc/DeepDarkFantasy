package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.ArrInfo
import com.thoughtworks.DDF.Top.TopInfo

trait StreamInfo[Info[_], Repr[_]] extends TopInfo[Info, Repr] with ArrInfo[Info, Repr] {
  implicit def streamInfo[A : Info]: Info[scala.Stream[A]]
}
