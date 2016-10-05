package com.thoughtworks.DDF.Combinators

trait Comb[Info[_], Repr[_]] extends
  SKIRepr[Info, Repr] with
  BCKWRepr[Info, Repr] with
  YRepr[Info, Repr] with
  Let[Info, Repr] with
  App[Info, Repr]
