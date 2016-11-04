package com.thoughtworks.DDF.State

import com.thoughtworks.DDF.Product.ProdMin

trait StateBase[Info[_], Repr[_]] extends ProdMin[Info, Repr] {
  final type State[S, A] = S => (A, S)

  final def stateInfo[S, A]: Info[S] => Info[A] => Info[State[S, A]] = s => a => aInfo(s, prodInfo(a, s))

  final def stateInfoS[S, A]: Info[State[S, A]] => Info[S] = domInfo

  final def stateInfoA[S, A]: Info[State[S, A]] => Info[A] = prodZroInfo.compose(rngInfo[S, (A, S)])
}
