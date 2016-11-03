package com.thoughtworks.DDF.State

import com.thoughtworks.DDF.Product.ProdMin

trait StateBase[Info[_], Repr[_]] extends ProdMin[Info, Repr] {
  type State[S, A] = S => (A, S)
}
