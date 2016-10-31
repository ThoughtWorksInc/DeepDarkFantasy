package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.{EvalO, EvalOMatch, NoInfo}

trait EvalOLang extends Lang[LangInfoG, EvalO] with LangTermLangInfo[EvalO] {
  val ltl = LangTermLang

  def eval[X]: EvalO[X] => X = x => x.l.apply[NoInfo, Lambda[X => X]](EvalMLang)

  def aeval[A, B]: LangTerm[A => B] => (EvalO[A] => EvalO[B]) => EvalO[A => B] = la => f =>
    new EvalO[A => B] {
      override def l: LangTerm[A => B] = la

      override def tmr: tm.ret = f

      override val tm: EvalOMatch.Aux[A => B, EvalO[A] => EvalO[B]] = AEM[A, B]
    }

  override def B[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]):
  EvalO[(B => C) => (A => B) => A => C] =
    aeval(ltl.B[A, B, C])(f => aeval(ltl.B_[A, B, C](f.l))(g => aeval(ltl.B__(f.l)(g.l))(x => app(f)(app(g)(x)))))

  def AEM[A, B]: EvalOMatch.Aux[A => B, EvalO[A] => EvalO[B]] = new EvalOMatch[A => B] {
    override type ret = EvalO[A] => EvalO[B]
  }

  override def app[A, B]: EvalO[A => B] => EvalO[A] => EvalO[B] = f => f.get(AEM[A, B])

  override def scanRight[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]):
  EvalO[(A => B => B) => B => List[A] => List[B]] = ???

  override def divD: EvalO[Double => Double => Double] =
    aeval(ltl.divD)(l => aeval(ltl.divD_(l.l))(r => litD(eval(l) / eval(r))))

  override def ltD: EvalO[Double => Double => Boolean] =
    aeval(ltl.ltD)(l => aeval(ltl.ltD_(l.l))(r => litB(eval(l) < eval(r))))

  override def scanLeft[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]):
  EvalO[(B => A => B) => B => List[A] => List[B]] = ???

  override def listZip[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[List[A] => List[B] => List[(A, B)]] = ???

  override def foldRight[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(A => B => B) => B => List[A] => B] = ???

  override def none[A](implicit ai: LangInfoG[A]): EvalO[Option[A]] = new EvalO[Option[A]] {
    override def l: LangTerm[Option[A]] = ltl.none[A]

    override def tmr: tm.ret = ()

    override val tm: EvalOMatch.Aux[Option[A], Unit] = new EvalOMatch[Option[A]] {
      override type ret = Unit
    }
  }

  override def some[A](implicit ai: LangInfoG[A]): EvalO[A => Option[A]] =
    aeval(ltl.some[A])(a => new EvalO[Option[A]] {
      override def l: LangTerm[Option[A]] = ltl.some_(a.l)

      override def tmr: tm.ret = ()

      override val tm: EvalOMatch.Aux[Option[A], Unit] = new EvalOMatch[Option[A]] {
        override type ret = Unit
      }
    })

  override def optionMatch[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(Option[A]) => (B) => ((A) => B) => B] = ???

  override def I[A](implicit ai: LangInfoG[A]): EvalO[A => A] = aeval(ltl.I[A])(identity[EvalO[A]])

  override def foldLeft[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(A => B => A) => A => List[B] => A] = ???

  override def listMap[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(A => B) => List[A] => List[B]] = ???

  override def reverse[A](implicit ai: LangInfoG[A]): EvalO[List[A] => List[A]] = ???

  override def nil[A](implicit ai: LangInfoG[A]): EvalO[List[A]] = ???

  override def cons[A](implicit ai: LangInfoG[A]): EvalO[A => List[A] => List[A]] = ???

  override def listMatch[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]):
  EvalO[List[A] => B => (A => List[A] => B) => B] = ???

  override def C[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]):
  EvalO[(A => B => C) => B => A => C] =
    aeval(ltl.C[A, B, C])(f => aeval(ltl.C_(f.l))(b => aeval(ltl.C__(f.l)(b.l))(a => app(app(f)(a))(b))))

  override def App[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(A => B) => A => B] = ???

  override def uncurry[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]): EvalO[((A) => (B) => C) => ((A, B)) => C] = ???

  override def plusD: EvalO[(Double) => (Double) => Double] = ???

  override def mkUnit: EvalO[Unit] = new EvalO[Unit] {
    override def l: LangTerm[Unit] = ltl.mkUnit

    override def tmr: tm.ret = ()

    override val tm: EvalOMatch.Aux[Unit, Unit] = new EvalOMatch[Unit] {
      override type ret = Unit
    }
  }

  override def sigD: EvalO[(Double) => Double] = ???

  override def multD: EvalO[Double => Double => Double] =
    aeval(ltl.multD)(l => aeval(ltl.multD_(l.l))(r => litD(eval(l) * eval(r))))

  override def mkProduct[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(A) => (B) => (A, B)] = ???

  override def zeroth[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[((A, B)) => A] = ???

  override def first[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[((A, B)) => B] = ???

  override def litD: Double => EvalO[Double] = d => new EvalO[Double] {
    override def l: LangTerm[Double] = ltl.litD(d)

    override def tmr: tm.ret = ()

    override val tm: EvalOMatch.Aux[Double, Unit] = new EvalOMatch[Double] {
      override type ret = Unit
    }
  }

  override def sumComm[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(Either[A, B]) => Either[B, A]] = ???

  override def sumAssocLR[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]): EvalO[(Either[Either[A, B], C]) => Either[A, Either[B, C]]] = ???

  override def sumAssocRL[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]): EvalO[(Either[A, Either[B, C]]) => Either[Either[A, B], C]] = ???

  override def left[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[A => Either[A, B]] = ???

  override def right[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[B => Either[A, B]] = ???

  override def sumMatch[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]):
  EvalO[(Either[A, B]) => ((A) => C) => ((B) => C) => C] = ???

  override def Y[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(((A) => B) => (A) => B) => (A) => B] = ???

  override def Let[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[A => (A => B) => B] = ???

  override def W[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[(A => A => B) => A => B] =
    aeval(ltl.W[A, B])(f => aeval(ltl.W_[A, B](f.l))(a => app(app(f)(a))(a)))

  override def K[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): EvalO[A => B => A] =
    aeval(ltl.K[A, B])(a => aeval(ltl.K_[A, B](a.l))(_ => a))

  override def curry[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]):
  EvalO[(((A, B)) => C) => A => B => C] = ???

  override def S[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]):
  EvalO[(A => B => C) => (A => B) => A => C] =
    aeval(ltl.S[A, B, C])(f => aeval(ltl.S_(f.l))(x => aeval(ltl.S__(f.l)(x.l))(a => app(app(f)(a))(app(x)(a)))))

  override def expD: EvalO[Double => Double] = aeval(ltl.expD)(x => litD(Math.exp(eval(x))))

  override def litB: Boolean => EvalO[Boolean] = b => new EvalO[Boolean] {
    override def l: LangTerm[Boolean] = ltl.litB(b)

    override def tmr: tm.ret = ()

    override val tm: EvalOMatch.Aux[Boolean, Unit] = new EvalOMatch[Boolean] {
      override type ret = Unit
    }
  }

  override def ite[A](implicit ai: LangInfoG[A]): EvalO[Boolean => A => A => A] = ???

  override def reprInfo[A]: EvalO[A] => LangInfoG[A] = x => ltl.reprInfo(x.l)
}

object EvalOLang extends EvalOLang