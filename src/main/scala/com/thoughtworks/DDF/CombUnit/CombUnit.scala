package com.thoughtworks.DDF.CombUnit

import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Unit.Unit

trait CombUnit[Info[_], Repr[_]] extends Comb[Info, Repr] with Unit[Info, Repr]
