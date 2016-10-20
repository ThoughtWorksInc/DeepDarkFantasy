package com.thoughtworks.DDF.CombUnit

import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Unit.UnitRepr

trait CombUnitL[Info[_], Repr[_]] extends Comb[Info, Repr] with UnitRepr[Info, Repr]
