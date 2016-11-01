package com.thoughtworks.DDF.Product

trait ProdMin[Info[_], Repr[_]] extends MkProd[Info, Repr] with Zeroth[Info, Repr] with First[Info, Repr]
