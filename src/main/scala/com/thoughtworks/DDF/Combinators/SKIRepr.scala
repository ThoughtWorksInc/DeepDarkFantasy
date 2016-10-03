package com.thoughtworks.DDF.Combinators

trait SKIRepr[Info[_], Repr[_]] extends SRepr[Info, Repr] with KRepr[Info, Repr] with IRepr[Info, Repr]



