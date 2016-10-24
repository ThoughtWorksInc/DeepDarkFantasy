package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Combinators.{Comb, ImpWComb}
import com.thoughtworks.DDF.Double.ImpWDouble
import com.thoughtworks.DDF.List.ImpWList
import com.thoughtworks.DDF.Option.ImpWOption
import com.thoughtworks.DDF.Sum.ImpWSum
import com.thoughtworks.DDF.Unit.Unit
import com.thoughtworks.DDF.{BEval, ImpW, Loss}

trait ImpWLang[Info[_], Repr[_]] extends
  Lang[Lambda[X => (Info[X], Loss[X])], ImpW[Info, Repr, ?]] with
  ImpWList[Info, Repr] with
  ImpWSum[Info, Repr] with
  ImpWComb[Info, Repr] with
  ImpWDouble[Info, Repr] with
  ImpWOption[Info, Repr] {
  override def base: Lang[Info, Repr]

  override def baseE: Lang[Loss, BEval] = BEvalLang.apply
}

object ImpWLang {
  implicit def apply[Info[_], Repr[_]]: ImpWLang[Info, Repr] = new ImpWLang[Info, Repr] {
    override implicit def unitInfo: (Info[scala.Unit], Loss[scala.Unit]) = ???

    override def runit: Unit[Info, Repr] = base

    override def rcomb: Comb[Info, Repr] = base

    override def litB: (Boolean) => ImpW[Info, Repr, Boolean] = ???

    override def ite[A](implicit ai: (Info[A], Loss[A])): ImpW[Info, Repr, Boolean => A => A => A] = ???

    override implicit def BoolInfo: (Info[Boolean], Loss[Boolean]) = ???

    override def mkUnit: ImpW[Info, Repr, scala.Unit] = ???

    override def base: Lang[Info, Repr] = ???
  }
}