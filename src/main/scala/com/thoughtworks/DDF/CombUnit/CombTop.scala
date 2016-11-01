package com.thoughtworks.DDF.CombUnit

import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Top.Top

trait CombTop[Info[_], Repr[_]] extends Comb[Info, Repr] with Top[Info, Repr]
