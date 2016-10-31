package com.thoughtworks.DDF.Cont

import com.thoughtworks.DDF.Arrow.ArrMin

trait ContBase[Info[_], Repr[_]] extends ArrMin[Info, Repr] {
  type Cont[R, A] = (A => R) => R
}
