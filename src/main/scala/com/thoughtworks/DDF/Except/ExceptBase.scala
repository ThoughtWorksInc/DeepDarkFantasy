package com.thoughtworks.DDF.Except

import com.thoughtworks.DDF.Sum.SumMin

trait ExceptBase[Info[_], Repr[_]] extends SumMin[Info, Repr] {
  type Except[A, B] = Either[A, B]
}
