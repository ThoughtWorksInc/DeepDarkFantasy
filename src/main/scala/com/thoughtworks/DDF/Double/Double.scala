package com.thoughtworks.DDF.Double

trait Double[Info[_], Repr[_]] extends
  PlusD[Info, Repr] with
  MultD[Info, Repr] with
  DivD[Info, Repr] with
  ExpD[Info, Repr] with
  SigD[Info, Repr] with
  LtD[Info, Repr]
