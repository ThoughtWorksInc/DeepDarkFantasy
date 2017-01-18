package com.thoughtworks.DDF.State

import com.thoughtworks.DDF.Product.ProdMin

trait StateBase[Info[_], Repr[_]] extends ProdMin[Info, Repr] {
  final type State[S, A] = S => (A, S)

  final def stateInfo[S, A]: Info[S] => Info[A] => Info[State[S, A]] = s => a => aInfo(s, prodInfo(a, s))
}
