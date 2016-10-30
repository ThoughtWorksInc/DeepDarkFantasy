package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.{EvalO, EvalOMatch}

class EvalOLang extends Lang[LangInfoG, EvalO] {
  val ltl = LangTermLang

  override def B[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]):
  EvalO[(B => C) => (A => B) => A => C] =
    new EvalO[(B => C) => (A => B) => (A) => C] {
      override def l: LangTerm[(B => C) => (A => B) => A => C] = ltl.B[A, B, C]

      override def tmr: tm.ret = f => new EvalO[(A => B) => A => C] {
        override def l: LangTerm[(A => B) => A => C] = ltl.B_[A, B, C](f.l)

        override def tmr: tm.ret = g => new EvalO[A => C] {
          override def l: LangTerm[A => C] = ltl.B__[A, B, C](f.l)(g.l)

          override def tmr: tm.ret = x => app(f)(app(g)(x))

          override val tm: EvalOMatch.Aux[A => C, EvalO[A] => EvalO[C]] = AEM[A, C]
        }

        override val tm: EvalOMatch.Aux[(A => B) => A => C, EvalO[A => B] => EvalO[A => C]] = AEM[A => B, A => C]
      }

      override val tm: EvalOMatch.Aux[(B => C) => (A => B) => A => C, EvalO[B => C] => EvalO[(A => B) => A => C]] =
        AEM[B => C, (A => B) => A => C]
    }

  def AEM[A, B]: EvalOMatch.Aux[A => B, EvalO[A] => EvalO[B]] = new EvalOMatch[A => B] {
    override type ret = EvalO[A] => EvalO[B]
  }

  override def app[A, B]: EvalO[A => B] => EvalO[A] => EvalO[B] = f => f.get(AEM[A, B])

  override def scanRight[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[((A) => (B) => B) => (B) => (List[A]) => List[B]] = ???

  override def divD: EvalO[(Double) => (Double) => Double] = ???

  override implicit def optionInfo[A](implicit ai: LangInfoG[A]): LangInfoG[Option[A]] = ???

  override def optionElmInfo[A]: (LangInfoG[Option[A]]) => LangInfoG[A] = ???

  override def ltD: EvalO[(Double) => (Double) => Boolean] = ???

  override def scanLeft[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[((B) => (A) => B) => (B) => (List[A]) => List[B]] = ???

  override def listZip[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(List[A]) => (List[B]) => List[(A, B)]] = ???

  override def foldRight[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[((A) => (B) => B) => (B) => (List[A]) => B] = ???

  override def none[A](implicit ai: LangInfoG[A]): EvalO[Option[A]] = ???

  override def some[A](implicit ai: LangInfoG[A]): EvalO[(A) => Option[A]] = ???

  override def optionMatch[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(Option[A]) => (B) => ((A) => B) => B] = ???

  override def I[A](implicit ai: LangInfoG[A]): EvalO[(A) => A] = ???

  override def foldLeft[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[((A) => (B) => A) => (A) => (List[B]) => A] = ???

  override implicit def aInfo[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): LangInfoG[(A) => B] = ???

  override def domInfo[A, B]: (LangInfoG[(A) => B]) => LangInfoG[A] = ???

  override def rngInfo[A, B]: (LangInfoG[(A) => B]) => LangInfoG[B] = ???

  override implicit def boolInfo: LangInfoG[Boolean] = ???

  override def listMap[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[((A) => B) => (List[A]) => List[B]] = ???

  override implicit def unitInfo: LangInfoG[Unit] = ???

  override def reverse[A](implicit ai: LangInfoG[A]): EvalO[(List[A]) => List[A]] = ???

  override def nil[A](implicit ai: LangInfoG[A]): EvalO[List[A]] = ???

  override def cons[A](implicit ai: LangInfoG[A]): EvalO[(A) => (List[A]) => List[A]] = ???

  override def listMatch[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(List[A]) => (B) => ((A) => (List[A]) => B) => B] = ???

  override def C[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]): EvalO[((A) => (B) => C) => (B) => (A) => C] = ???

  override def App[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[((A) => B) => (A) => B] = ???

  override def uncurry[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]): EvalO[((A) => (B) => C) => ((A, B)) => C] = ???

  override def plusD: EvalO[(Double) => (Double) => Double] = ???

  override def mkUnit: EvalO[Unit] = ???

  override def sigD: EvalO[(Double) => Double] = ???

  override def multD: EvalO[(Double) => (Double) => Double] = ???

  override implicit def doubleInfo: LangInfoG[Double] = ???

  override def mkProduct[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(A) => (B) => (A, B)] = ???

  override def zeroth[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[((A, B)) => A] = ???

  override def first[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[((A, B)) => B] = ???

  override def litD: (Double) => EvalO[Double] = ???

  override def sumComm[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(Either[A, B]) => Either[B, A]] = ???

  override def sumAssocLR[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]): EvalO[(Either[Either[A, B], C]) => Either[A, Either[B, C]]] = ???

  override def sumAssocRL[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]): EvalO[(Either[A, Either[B, C]]) => Either[Either[A, B], C]] = ???

  override def left[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(A) => Either[A, B]] = ???

  override def right[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(B) => Either[A, B]] = ???

  override def sumMatch[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]): EvalO[(Either[A, B]) => ((A) => C) => ((B) => C) => C] = ???

  override implicit def sumInfo[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): LangInfoG[Either[A, B]] = ???

  override def sumLeftInfo[A, B]: (LangInfoG[Either[A, B]]) => LangInfoG[A] = ???

  override def sumRightInfo[A, B]: (LangInfoG[Either[A, B]]) => LangInfoG[B] = ???

  override def Y[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(((A) => B) => (A) => B) => (A) => B] = ???

  override def Let[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(A) => ((A) => B) => B] = ???

  override def W[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[((A) => (A) => B) => (A) => B] = ???

  override def K[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(A) => (B) => A] = ???

  override def curry[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]): EvalO[(((A, B)) => C) => (A) => (B) => C] = ???

  override def S[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]): EvalO[((A) => (B) => C) => ((A) => B) => (A) => C] = ???

  override implicit def productInfo[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): LangInfoG[(A, B)] = ???

  override def productZerothInfo[A, B]: (LangInfoG[(A, B)]) => LangInfoG[A] = ???

  override def productFirstInfo[A, B]: (LangInfoG[(A, B)]) => LangInfoG[B] = ???

  override def expD: EvalO[(Double) => Double] = ???

  override def litB: (Boolean) => EvalO[Boolean] = ???

  override def ite[A](implicit ai: LangInfoG[A]): EvalO[(Boolean) => (A) => (A) => A] = ???

  override implicit def listInfo[A](implicit ai: LangInfoG[A]): LangInfoG[List[A]] = ???

  override def listElmInfo[A]: (LangInfoG[List[A]]) => LangInfoG[A] = ???

  override def reprInfo[A]: (EvalO[A]) => LangInfoG[A] = ???
}
