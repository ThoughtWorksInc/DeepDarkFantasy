package com.thoughtworks.DDF.Double

trait Double[Info[_], Repr[_]] extends
  DoubleMin[Info, Repr] with
  NegD[Info, Repr] with
  MinusD[Info, Repr] with
  DivD[Info, Repr]
