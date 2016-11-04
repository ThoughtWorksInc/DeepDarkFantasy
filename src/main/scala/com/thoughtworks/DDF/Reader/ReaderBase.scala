package com.thoughtworks.DDF.Reader

import com.thoughtworks.DDF.Arrow.ArrMin

trait ReaderBase[Info[_], Repr[_]] extends ArrMin[Info, Repr] {
  final type Reader[E, T] = E => T

  final def readerInfo[E, T]: Info[E] => Info[T] => Info[Reader[E, T]] = e => t => aInfo(e, t)

  final def readerInfoE[E, T]: Info[Reader[E, T]] => Info[E] = domInfo

  final def readerInfoT[E, T]: Info[Reader[E, T]] => Info[T] = rngInfo
}
