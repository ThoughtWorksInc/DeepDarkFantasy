package com.thoughtworks.DDF.CombUnit

import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Unit.UnitLang

trait CombUnit[Info[_], Repr[_]] extends Comb[Info, Repr] with UnitLang[Info, Repr]
