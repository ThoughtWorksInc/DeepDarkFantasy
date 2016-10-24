package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Bool.ImpWBool
import com.thoughtworks.DDF.Combinators.{Comb, ImpWComb}
import com.thoughtworks.DDF.Double.ImpWDouble
import com.thoughtworks.DDF.List.ImpWList
import com.thoughtworks.DDF.Option.ImpWOption
import com.thoughtworks.DDF.Sum.ImpWSum
import com.thoughtworks.DDF.Unit.{ImpWUnit, Unit}
import com.thoughtworks.DDF.{BEval, ImpW, Loss}

trait ImpWLang[Info[_], Repr[_]] extends
  Lang[Lambda[X => (Info[X], Loss[X])], ImpW[Info, Repr, ?]] with
  ImpWList[Info, Repr] with
  ImpWSum[Info, Repr] with
  ImpWComb[Info, Repr] with
  ImpWDouble[Info, Repr] with
  ImpWOption[Info, Repr] with
  ImpWUnit[Info, Repr] with
  ImpWBool[Info, Repr] {
  override def base: Lang[Info, Repr]

  override def baseE: Lang[Loss, BEval] = BEvalLang.apply

  override def rcomb: Comb[Info, Repr] = base
}

object ImpWLang {
  implicit def apply[Info[_], Repr[_]](implicit l: Lang[Info, Repr]): ImpWLang[Info, Repr] = new ImpWLang[Info, Repr] {
    override def base: Lang[Info, Repr] = l
  }
}