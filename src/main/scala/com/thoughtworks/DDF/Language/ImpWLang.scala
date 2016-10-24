package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Combinators.{Comb, ImpWComb}
import com.thoughtworks.DDF.Double.ImpWDouble
import com.thoughtworks.DDF.List.{ImpWList, List}
import com.thoughtworks.DDF.Sum.{ImpWSum, Sum}
import com.thoughtworks.DDF.Unit.Unit
import com.thoughtworks.DDF.{BEval, ImpW, Loss}

trait ImpWLang[Info[_], Repr[_]] extends
  Lang[Lambda[X => (Info[X], Loss[X])], ImpW[Info, Repr, ?]] with
  ImpWList[Info, Repr] with
  ImpWSum[Info, Repr] with
  ImpWComb[Info, Repr] with
  ImpWDouble[Info, Repr] {
  override def base: Lang[Info, Repr]

  override def baseE: Lang[Loss, BEval] = BEvalLang.apply
}

object ImpWLang {
  implicit def apply[Info[_], Repr[_]]: ImpWLang[Info, Repr] = new ImpWLang[Info, Repr] {
    override implicit def optionInfo[A](implicit ai: (Info[A], Loss[A])): (Info[Option[A]], Loss[Option[A]]) = ???

    override def optionElmInfo[A]: ((Info[Option[A]], Loss[Option[A]])) => (Info[A], Loss[A]) = ???

    override implicit def unitInfo: (Info[scala.Unit], Loss[scala.Unit]) = ???

    override def runit: Unit[Info, Repr] = base

    override def rcomb: Comb[Info, Repr] = base

    override implicit def doubleInfo: (Info[Double], Loss[Double]) = ???

    override def litB: (Boolean) => ImpW[Info, Repr, Boolean] = ???

    override def ite[A](implicit ai: (Info[A], Loss[A])): ImpW[Info, Repr, (Boolean) => (A) => (A) => A] = ???

    override implicit def BoolInfo: (Info[Boolean], Loss[Boolean]) = ???

    override def mkUnit: ImpW[Info, Repr, scala.Unit] = ???

    override def none[A](implicit ai: (Info[A], Loss[A])): ImpW[Info, Repr, Option[A]] = ???

    override def some[A](implicit ai: (Info[A], Loss[A])): ImpW[Info, Repr, A => Option[A]] = ???

    override def optionMatch[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])): ImpW[Info, Repr, Option[A] => B => (A => B) => B] = ???

    override def base: Lang[Info, Repr] = ???
  }
}