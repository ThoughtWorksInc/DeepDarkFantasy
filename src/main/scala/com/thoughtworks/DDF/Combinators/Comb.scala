package com.thoughtworks.DDF.Combinators

trait Comb[Info[_], Repr[_]] extends
  SKI[Info, Repr] with
  BCKW[Info, Repr] with
  Y[Info, Repr] with
  Let[Info, Repr] with
  App[Info, Repr]
