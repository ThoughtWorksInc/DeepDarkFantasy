package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.List.{ImpWList, List}
import com.thoughtworks.DDF.Sum.ImpWSum
import com.thoughtworks.DDF.Unit.Unit
import com.thoughtworks.DDF.{ImpW, Loss}

trait ImpWLang[Info[_], Repr[_]] extends
  Lang[Lambda[X => (Info[X], Loss[X])], ImpW[Info, Repr, ?]] with
  ImpWList[Info, Repr] with
  ImpWSum[Info, Repr] {
}

object ImpWLang {
  implicit def apply[Info[_], Repr[_]]: ImpWLang[Info, Repr] = new ImpWLang[Info, Repr] {
    override implicit def optionInfo[A](implicit ai: (Info[A], Loss[A])): (Info[Option[A]], Loss[Option[A]]) = ???

    override def optionElmInfo[A]: ((Info[Option[A]], Loss[Option[A]])) => (Info[A], Loss[A]) = ???

    override def Y[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])): ImpW[Info, Repr, (((A) => B) => (A) => B) => (A) => B] = ???

    override implicit def unitInfo: (Info[scala.Unit], Loss[scala.Unit]) = ???

    override def C[A, B, C](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B]), ci: (Info[C], Loss[C])): ImpW[Info, Repr, ((A) => (B) => C) => (B) => (A) => C] = ???

    override def I[A](implicit ai: (Info[A], Loss[A])): ImpW[Info, Repr, (A) => A] = ???

    override def W[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])): ImpW[Info, Repr, ((A) => (A) => B) => (A) => B] = ???

    override def runit: Unit[Info, Repr] = ???

    override def B[A, B, C](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B]), ci: (Info[C], Loss[C])): ImpW[Info, Repr, ((B) => C) => ((A) => B) => (A) => C] = ???

    override def rcomb: Comb[Info, Repr] = ???

    override def K[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])): ImpW[Info, Repr, (A) => (B) => A] = ???

    override implicit def doubleInfo: (Info[Double], Loss[Double]) = ???

    override def Let[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])): ImpW[Info, Repr, (A) => ((A) => B) => B] = ???

    override def litB: (Boolean) => ImpW[Info, Repr, Boolean] = ???

    override def ite[A](implicit ai: (Info[A], Loss[A])): ImpW[Info, Repr, (Boolean) => (A) => (A) => A] = ???

    override def litD: (Double) => ImpW[Info, Repr, Double] = ???

    override def plusD: ImpW[Info, Repr, (Double) => (Double) => Double] = ???

    override def multD: ImpW[Info, Repr, (Double) => (Double) => Double] = ???

    override def divD: ImpW[Info, Repr, (Double) => (Double) => Double] = ???

    override def expD: ImpW[Info, Repr, (Double) => Double] = ???

    override def sigD: ImpW[Info, Repr, (Double) => Double] = ???

    override def App[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])): ImpW[Info, Repr, ((A) => B) => (A) => B] = ???

    override def S[A, B, C](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B]), ci: (Info[C], Loss[C])): ImpW[Info, Repr, ((A) => (B) => C) => ((A) => B) => (A) => C] = ???

    override def left[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])): ImpW[Info, Repr, (A) => Either[A, B]] = ???

    override def right[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])): ImpW[Info, Repr, (B) => Either[A, B]] = ???

    override def sumMatch[A, B, C](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B]), ci: (Info[C], Loss[C])): ImpW[Info, Repr, (Either[A, B]) => ((A) => C) => ((B) => C) => C] = ???

    override implicit def BoolInfo: (Info[Boolean], Loss[Boolean]) = ???

    override def mkUnit: ImpW[Info, Repr, scala.Unit] = ???

    override def base: List[Info, Repr] = ???

    override def none[A](implicit ai: (Info[A], Loss[A])): ImpW[Info, Repr, Option[A]] = ???

    override def some[A](implicit ai: (Info[A], Loss[A])): ImpW[Info, Repr, (A) => Option[A]] = ???

    override def optionMatch[A, B](implicit ai: (Info[A], Loss[A]), bi: (Info[B], Loss[B])): ImpW[Info, Repr, (Option[A]) => (B) => ((A) => B) => B] = ???
  }
}