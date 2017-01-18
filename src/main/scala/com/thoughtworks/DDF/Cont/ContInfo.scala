package com.thoughtworks.DDF.Cont

import com.thoughtworks.DDF.Arrow.ArrInfo

trait ContInfo[Info[_], Repr[_]] extends ArrInfo[Info, Repr] {
  final type Cont[R, A] = (A => R) => R

  final def contInfo[R, A]: Info[R] => Info[A] => Info[Cont[R, A]] = r => a => aInfo(aInfo(a, r), r)

  final def contInfoR[R, A]: Info[Cont[R, A]] => Info[R] = rngInfo

  final def contInfoA[R, A]: Info[Cont[R, A]] => Info[A] = domInfo.compose(domInfo[(A => R), R])
}
