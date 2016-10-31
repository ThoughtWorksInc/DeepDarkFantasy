package com.thoughtworks.DDF.Product

trait ProductMin[Info[_], Repr[_]] extends MkProduct[Info, Repr] with Zeroth[Info, Repr] with First[Info, Repr]
