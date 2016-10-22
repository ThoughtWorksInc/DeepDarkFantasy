package com.thoughtworks.DDF.Combinators

trait BCKW[Info[_], Repr[_]] extends
  B[Info, Repr] with
  C[Info, Repr] with
  K[Info, Repr] with
  W[Info, Repr]
