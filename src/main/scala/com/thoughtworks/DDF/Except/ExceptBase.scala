package com.thoughtworks.DDF.Except

import com.thoughtworks.DDF.Sum.SumMin

trait ExceptBase extends SumMin {
  final type Except[A, B] = Either[A, B]

  final def exceptInfo[A, B]: Info[A] => Info[B] => Info[Except[A, B]] = a => b => sumInfo(a, b)

  final def exceptInfoA[A, B]: Info[Except[A, B]] => Info[A] = sumLeftInfo[A, B]

  final def exceptInfoB[A, B]: Info[Except[A, B]] => Info[B] = sumRightInfo[A, B]
}
