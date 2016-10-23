package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Combinators.{Comb, ImpWComb}
import com.thoughtworks.DDF.List.{ImpWList, List}
import com.thoughtworks.DDF.Sum.{ImpWSum, Sum}
import com.thoughtworks.DDF.Unit.Unit
import com.thoughtworks.DDF.{BEval, ImpW, Loss}

trait ImpWLang[Info[_], Repr[_]] extends
  Lang[Lambda[X => (Info[X], Loss[X])], ImpW[Info, Repr, ?]] with
  ImpWList[Info, Repr] with
  ImpWSum[Info, Repr] with
  ImpWComb[Info, Repr] {
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

    override def litD: (Double) => ImpW[Info, Repr, Double] = ???

    override def plusD: ImpW[Info, Repr, (Double) => (Double) => Double] = ???

    override def multD: ImpW[Info, Repr, (Double) => (Double) => Double] = ???

    override def divD: ImpW[Info, Repr, (Double) => (Double) => Double] = ???

    override def expD: ImpW[Info, Repr, (Double) => Double] = ???

    override def sigD: ImpW[Info, Repr, (Double) => Double] = ???

    override implicit def BoolInfo: (Info[Boolean], Loss[Boolean]) = ???

    override def mkUnit: ImpW[Info, Repr, scala.Unit] = ???

    override def none[A](implicit ai: (Info[A], Loss[A])): ImpW[Info, Repr, Option[A]] = ???

    override def some[A](implicit ai: (Info[A], Loss[A])): ImpW[Info, Repr, A => Option[A]] = ???

    override def optionMatch[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])): ImpW[Info, Repr, Option[A] => B => (A => B) => B] = ???

    override def base: Lang[Info, Repr] = ???
  }
}