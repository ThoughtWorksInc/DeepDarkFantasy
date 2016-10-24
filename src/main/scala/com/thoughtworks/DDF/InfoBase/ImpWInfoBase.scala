package com.thoughtworks.DDF.InfoBase

import com.thoughtworks.DDF.CombUnit.{CombUnit, CombUnitExt}
import com.thoughtworks.DDF.Combinators.{BEvalComb, Comb}
import com.thoughtworks.DDF.Unit.{BEvalUnit, Unit}
import com.thoughtworks.DDF.{BEval, ImpW, Loss}

trait ImpWInfoBase[Info[_], Repr[_]] extends
  InfoBase[Lambda[X => (Info[X], Loss[X])], ImpW[Info, Repr, ?]] {

  def becomb: Comb[Loss, BEval] = BEvalComb.apply

  def becun: CombUnit[Loss, BEval] = CombUnitExt.apply(becomb, BEvalUnit.apply)

  def rcomb: Comb[Info, Repr]

  def runit: Unit[Info, Repr]

  def rcun: CombUnit[Info, Repr] = CombUnitExt.apply(rcomb, runit)

  override def reprInfo[A]: ImpW[Info, Repr, A] => (Info[A], Loss[A]) = x =>
    (rcomb.arrowRangeInfo(rcomb.reprInfo(x.exp)), becomb.arrowRangeInfo(becomb.reprInfo(x.eval)))
}

object ImpWInfoBase {
  implicit def apply[Info[_], Repr[_]](implicit c: Comb[Info, Repr], u: Unit[Info, Repr]):
  ImpWInfoBase[Info, Repr] = new ImpWInfoBase[Info, Repr] {
    override def rcomb: Comb[Info, Repr] = c

    override def runit: Unit[Info, Repr] = u
  }
}