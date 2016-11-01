package com.thoughtworks.DDF.Product

trait Prod[Info[_], Repr[_]] extends
  Curry[Info, Repr] with
  UnCurry[Info, Repr]
