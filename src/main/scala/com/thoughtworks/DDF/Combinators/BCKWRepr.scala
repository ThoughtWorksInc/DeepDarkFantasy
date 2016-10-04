package com.thoughtworks.DDF.Combinators

trait BCKWRepr[Info[_], Repr[_]] extends
  BaseRepr[Info, Repr] with
  CRepr[Info, Repr] with
  KRepr[Info, Repr] with
  WRepr[Info, Repr]
