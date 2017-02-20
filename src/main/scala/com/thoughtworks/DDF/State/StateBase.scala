package com.thoughtworks.DDF.State

import com.thoughtworks.DDF.Product.ProdMin

trait StateBase extends ProdMin {
  final type State[S <: Type, +A <: Type] = S ~>: Prod[A, S]
}
