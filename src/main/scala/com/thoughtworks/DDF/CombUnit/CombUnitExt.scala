package com.thoughtworks.DDF.CombUnit

import com.thoughtworks.DDF.Combinators.{Comb, CombExt}
import com.thoughtworks.DDF.Unit.{Unit, UnitExt}

trait CombUnitExt[Info[_], Repr[_]] extends CombUnit[Info, Repr] with CombExt[Info, Repr] with UnitExt[Info, Repr] {}

object CombUnitExt {
  implicit def apply[Info[_], Repr[_]](implicit c: Comb[Info, Repr], u: Unit[Info, Repr]):
  CombUnit[Info, Repr] = new CombUnitExt[Info, Repr] {
    override val comb: Comb[Info, Repr] = c

    override val unit: Unit[Info, Repr] = u
  }
}