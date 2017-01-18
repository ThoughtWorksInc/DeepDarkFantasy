package com.thoughtworks.DDF.Except

import com.thoughtworks.DDF.Sum.SumMin

trait ExceptBase[Info[_], Repr[_]] extends SumMin[Info, Repr] {
  final type Except[A, B] = Either[A, B]

  final def exceptInfo[A, B]: Info[A] => Info[B] => Info[Except[A, B]] = a => b => sumInfo(a, b)
}
