package com.thoughtworks.DDF.Reader

import com.thoughtworks.DDF.Arrow.Arr

trait ReaderBase[Info[_], Repr[_]] extends Arr[Info, Repr] {
  final type Reader[E, T] = E => T

  final def readerInfo[E, T]: Info[E] => Info[T] => Info[Reader[E, T]] = e => t => aInfo(e, t)
}
