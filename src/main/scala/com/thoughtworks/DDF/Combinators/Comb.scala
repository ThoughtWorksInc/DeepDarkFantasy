package com.thoughtworks.DDF.Combinators

trait Comb[Info[_], Repr[_]] extends SKIRepr[Info, Repr] with BCKWRepr[Info, Repr] with YRepr[Info, Repr]
