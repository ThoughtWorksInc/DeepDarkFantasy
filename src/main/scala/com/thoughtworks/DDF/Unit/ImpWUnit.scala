package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.InfoBase.ImpWInfoBase
import com.thoughtworks.DDF.{BEval, ImpW, Loss}

trait ImpWUnit[Info[_], Repr[_]] extends
  Unit[Lambda[X => (Info[X], Loss[X])], ImpW[Info, Repr, ?]] with
  ImpWInfoBase[Info, Repr] {
  def base: Unit[Info, Repr]

  def baseE: Unit[Loss, BEval] = BEvalUnit.apply

  override implicit def unitInfo = (base.unitInfo, baseE.unitInfo)

  override def mkUnit = ImpW(base.mkUnit, baseE.mkUnit)(rcun, becun)
}

object ImpWUnit {
  implicit def apply[Info[_], Repr[_]](implicit u: Unit[Info, Repr], c: Comb[Info, Repr]):
  ImpWUnit[Info, Repr] = new ImpWUnit[Info, Repr] {
    override def rcomb: Comb[Info, Repr] = c

    override def runit: Unit[Info, Repr] = base

    override def base: Unit[Info, Repr] = u
  }
}