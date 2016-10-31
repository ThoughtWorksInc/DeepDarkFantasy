package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.{FEMMatch, FEval, FEvalMatch}

trait FEvalInterLang extends InterLang[FEvalMatch, FEval] {
  val ltl = LangTermLang

  override def sumAssocRL[A, B, C](implicit ai: FEvalMatch[A], bi: FEvalMatch[B], ci: FEvalMatch[C]): FEval[(Either[A, Either[B, C]]) => Either[Either[A, B], C]] = ???

  override implicit def aInfo[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEvalMatch[(A) => B] = ???

  override def domInfo[A, B]: FEvalMatch[A => B] => FEvalMatch[A] = ???

  override def rngInfo[A, B]: FEvalMatch[A => B] => FEvalMatch[B] = ???

  override implicit def unitInfo: FEvalMatch[Unit] = new FEvalMatch[Unit] {
    override def gSelf: LangTerm[Unit => Unit] = ltl.I(LI)

    override def selfG: LangTerm[Unit => Unit] = ltl.I(LI)

    override type ret = Unit

    override def tmr: tm.ret = ()

    override val tm: FEMMatch.Aux[Unit, Unit] = new FEMMatch[Unit] {
      override type ret = Unit
    }

    override def LI: LangInfoG[Unit] = ltl.unitInfo
  }

  override def scanRight[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[((A) => (B) => B) => (B) => (List[A]) => List[B]] = ???

  override def listMap[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[((A) => B) => (List[A]) => List[B]] = ???

  override def scanLeft[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[((B) => (A) => B) => (B) => (List[A]) => List[B]] = ???

  override def listZip[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[(List[A]) => (List[B]) => List[(A, B)]] = ???

  override def zeroth[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[((A, B)) => A] = ???

  override def C[A, B, C](implicit ai: FEvalMatch[A], bi: FEvalMatch[B], ci: FEvalMatch[C]): FEval[((A) => (B) => C) => (B) => (A) => C] = ???

  override def App[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[((A) => B) => (A) => B] = ???

  override def uncurry[A, B, C](implicit ai: FEvalMatch[A], bi: FEvalMatch[B], ci: FEvalMatch[C]): FEval[((A) => (B) => C) => ((A, B)) => C] = ???

  override def I[A](implicit ai: FEvalMatch[A]): FEval[(A) => A] = ???

  override def reverse[A](implicit ai: FEvalMatch[A]): FEval[(List[A]) => List[A]] = ???

  override def cons[A](implicit ai: FEvalMatch[A]): FEval[(A) => (List[A]) => List[A]] = ???

  override def app[A, B]: (FEval[(A) => B]) => (FEval[A]) => FEval[B] = ???

  override implicit def optionInfo[A](implicit ai: FEvalMatch[A]): FEvalMatch[Option[A]] = ???

  override def optionElmInfo[A]: (FEvalMatch[Option[A]]) => FEvalMatch[A] = ???

  override def sumComm[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[(Either[A, B]) => Either[B, A]] = ???

  override def right[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[(B) => Either[A, B]] = ???

  override def ltD: FEval[(Double) => (Double) => Boolean] = ???

  override def expD: FEval[(Double) => Double] = ???

  override def divD: FEval[(Double) => (Double) => Double] = ???

  override def foldRight[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[((A) => (B) => B) => (B) => (List[A]) => B] = ???

  override def none[A](implicit ai: FEvalMatch[A]): FEval[Option[A]] = ???

  override def some[A](implicit ai: FEvalMatch[A]): FEval[(A) => Option[A]] = ???

  override def optionMatch[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[(Option[A]) => (B) => ((A) => B) => B] = ???

  override def first[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[((A, B)) => B] = ???

  override def litB: (Boolean) => FEval[Boolean] = ???

  override def mkUnit: FEval[Unit] = ???

  override implicit def sumInfo[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEvalMatch[Either[A, B]] = ???

  override def sumLeftInfo[A, B]: (FEvalMatch[Either[A, B]]) => FEvalMatch[A] = ???

  override def sumRightInfo[A, B]: (FEvalMatch[Either[A, B]]) => FEvalMatch[B] = ???

  override def Y[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[(((A) => B) => (A) => B) => (A) => B] = ???

  override implicit def doubleInfo: FEvalMatch[Double] = ???

  override def Let[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[(A) => ((A) => B) => B] = ???

  override def nil[A](implicit ai: FEvalMatch[A]): FEval[List[A]] = ???

  override def listMatch[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[(List[A]) => (B) => ((A) => (List[A]) => B) => B] = ???

  override def W[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[((A) => (A) => B) => (A) => B] = ???

  override def sumMatch[A, B, C](implicit ai: FEvalMatch[A], bi: FEvalMatch[B], ci: FEvalMatch[C]): FEval[(Either[A, B]) => ((A) => C) => ((B) => C) => C] = ???

  override def ite[A](implicit ai: FEvalMatch[A]): FEval[(Boolean) => (A) => (A) => A] = ???

  override implicit def boolInfo: FEvalMatch[Boolean] = ???

  override def sigD: FEval[(Double) => Double] = ???

  override def plusD: FEval[(Double) => (Double) => Double] = ???

  override def multD: FEval[(Double) => (Double) => Double] = ???

  override def K[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[(A) => (B) => A] = ???

  override def foldLeft[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[((A) => (B) => A) => (A) => (List[B]) => A] = ???

  override def curry[A, B, C](implicit ai: FEvalMatch[A], bi: FEvalMatch[B], ci: FEvalMatch[C]): FEval[(((A, B)) => C) => (A) => (B) => C] = ???

  override def left[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[(A) => Either[A, B]] = ???

  override def S[A, B, C](implicit ai: FEvalMatch[A], bi: FEvalMatch[B], ci: FEvalMatch[C]): FEval[((A) => (B) => C) => ((A) => B) => (A) => C] = ???

  override implicit def productInfo[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEvalMatch[(A, B)] = ???

  override def productZerothInfo[A, B]: (FEvalMatch[(A, B)]) => FEvalMatch[A] = ???

  override def productFirstInfo[A, B]: (FEvalMatch[(A, B)]) => FEvalMatch[B] = ???

  override def sumAssocLR[A, B, C](implicit ai: FEvalMatch[A], bi: FEvalMatch[B], ci: FEvalMatch[C]): FEval[(Either[Either[A, B], C]) => Either[A, Either[B, C]]] = ???

  override def B[A, B, C](implicit ai: FEvalMatch[A], bi: FEvalMatch[B], ci: FEvalMatch[C]): FEval[((B) => C) => ((A) => B) => (A) => C] = ???

  override implicit def listInfo[A](implicit ai: FEvalMatch[A]): FEvalMatch[List[A]] = ???

  override def listElmInfo[A]: (FEvalMatch[List[A]]) => FEvalMatch[A] = ???

  override def litD: (Double) => FEval[Double] = ???

  override def mkProduct[A, B](implicit ai: FEvalMatch[A], bi: FEvalMatch[B]): FEval[(A) => (B) => (A, B)] = ???

  override def reprInfo[A]: (FEval[A]) => FEvalMatch[A] = ???
}

object FEvalInterLang extends FEvalInterLang