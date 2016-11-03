package com.thoughtworks.DDF.Reader

import com.thoughtworks.DDF.Arrow.ArrMin

trait ReaderBase[Info[_], Repr[_]] extends ArrMin[Info, Repr] {
  type Reader[E, T] = E => T
}
