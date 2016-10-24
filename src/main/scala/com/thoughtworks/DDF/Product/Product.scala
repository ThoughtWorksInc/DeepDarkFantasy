package com.thoughtworks.DDF.Product

trait Product[Info[_], Repr[_]] extends
  Curry[Info, Repr] with
  UnCurry[Info, Repr]
