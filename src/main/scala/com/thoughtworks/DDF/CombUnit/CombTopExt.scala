package com.thoughtworks.DDF.CombUnit

import com.thoughtworks.DDF.Combinators.{Comb, CombExt}
import com.thoughtworks.DDF.Top.{Top, TopExt}

trait CombTopExt[Info[_], Repr[_]] extends CombTop[Info, Repr] with CombExt[Info, Repr] with TopExt[Info, Repr] {}

object CombTopExt {
  implicit def apply[Info[_], Repr[_]](implicit c: Comb[Info, Repr], u: Top[Info, Repr]):
  CombTop[Info, Repr] = new CombTopExt[Info, Repr] {
    override val comb: Comb[Info, Repr] = c

    override val unit: Top[Info, Repr] = u
  }
}