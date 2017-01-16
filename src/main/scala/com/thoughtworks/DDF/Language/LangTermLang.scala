package com.thoughtworks.DDF.Language

trait LangTermLang extends Lang[LangInfoG, LangTerm] {
  override def ite[A](implicit ai: LangInfoG[A]) = new LangTerm[Boolean => A => A => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.ite[A](ai(lang))
  }

  override def K[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new LangTerm[A => B => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.K[A, B](ai(lang), bi(lang))
  }

  override def multD = new LangTerm[Double => Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.multD
  }

  override def some[A](implicit ai: LangInfoG[A]) = new LangTerm[A => Option[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.some[A](ai(lang))
  }

  override def reverse[A](implicit ai: LangInfoG[A]) = new LangTerm[List[A] => List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.reverse[A](ai(lang))
  }

  override def foldLeft[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[(A => B => A) => A => List[B] => A] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.foldLeft[A, B](ai(lang), bi(lang))
    }

  override def cons[A](implicit ai: LangInfoG[A]) = new LangTerm[A => List[A] => List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.cons[A](ai(lang))
  }

  override def expD = new LangTerm[Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.expD
  }

  override def exceptBind[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new LangTerm[Except[A, B] => (B => Except[A, C]) => Except[A, C]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.exceptBind(ai(lang), bi(lang), ci(lang))
    }

  override def mkProd[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new LangTerm[A => B => (A, B)] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.mkProd(ai(lang), bi(lang))
  }

  override def app[A, B]: LangTerm[A => B] => LangTerm[A] => LangTerm[B] = f => x => new LangTerm[B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[B] = lang.app(f(lang))(x(lang))
  }

  override implicit def listInfo[A](implicit ai: LangInfoG[A]) = new LangInfoG[List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[List[A]] = lang.listInfo(ai(lang))
  }

  override def listElmInfo[A]: LangInfoG[List[A]] => LangInfoG[A] = x => new LangInfoG[A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[A] = lang.listElmInfo(x(lang))
  }

  override def impossible = new LangTerm[Unit => Nothing] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.impossible
  }

  override def contBind[R, A, B](implicit ri: LangInfoG[R], ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[Cont[R, A] => (A => Cont[R, B]) => Cont[R, B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.contBind(ri(lang), ai(lang), bi(lang))
    }

  override def putDouble = new LangTerm[Double => IO[Unit]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.putDouble
  }

  override def I[A](implicit ai: LangInfoG[A]) = new LangTerm[A => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.I[A](ai(lang))
  }

  override def listMap[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[(A => B) => List[A] => List[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.listMap(ai(lang), bi(lang))
    }

  override def scanRight[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[(A => B => B) => B => List[A] => List[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.scanRight(ai(lang), bi(lang))
    }

  override def litString: String => LangTerm[String] = str => new LangTerm[String] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[String] = lang.litString(str)
  }

  override def imfalso[A](implicit ai: LangInfoG[A]) = new LangTerm[Unit => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.imfalso[A](ai(lang))
  }

  override def sumMatch[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new LangTerm[Either[A, B] => (A => C) => (B => C) => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.sumMatch(ai(lang), bi(lang), ci(lang))
    }

  override def left[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new LangTerm[A => Either[A, B]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.left[A, B](ai(lang), bi(lang))
  }

  override def stringInfo: LangInfoG[String] = new LangInfoG[String] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[String] = lang.stringInfo
  }

  override def stateBind[S, A, B](implicit si: LangInfoG[S], ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[State[S, A] => (A => State[S, B]) => State[S, B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.stateBind(si(lang), ai(lang), bi(lang))
    }

  override def exfalso[A](implicit ai: LangInfoG[A]) = new LangTerm[Nothing => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.exfalso[A](ai(lang))
  }

  override def zro[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new LangTerm[((A, B)) => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.zro(ai(lang), bi(lang))
  }

  override def plusD = new LangTerm[Double => Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.plusD
  }

  override def streamMatch[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[Stream[A] => B => (A => Stream[A] => B) => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.streamMatch(ai(lang), bi(lang))
    }

  override def ><[A, B, C, D](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C], di: LangInfoG[D]) =
    new LangTerm[(A => C) => (B => D) => ((A, B)) => (C, D)] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.`><`(ai(lang), bi(lang), ci(lang), di(lang))
    }

  override def readerBind[E, A, B](implicit ei: LangInfoG[E], ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[Reader[E, A] => (A => Reader[E, B]) => Reader[E, B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.readerBind(ei(lang), ai(lang), bi(lang))
    }

  override def none[A](implicit ai: LangInfoG[A]): LangTerm[Option[A]] = new LangTerm[Option[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[Option[A]] = lang.none[A](ai(lang))
  }

  override def foldRight[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[(A => B => B) => B => List[A] => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.foldRight[A, B](ai(lang), bi(lang))
    }

  override def IOBind[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[IO[A] => (A => IO[B]) => IO[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.IOBind(ai(lang), bi(lang))
    }

  override def fst[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new LangTerm[((A, B)) => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.fst(ai(lang), bi(lang))
  }

  override implicit def optionInfo[A](implicit ai: LangInfoG[A]) = new LangInfoG[Option[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.optionInfo(ai(lang))
  }

  override def optionElmInfo[A]: LangInfoG[Option[A]] => LangInfoG[A] = ai => new LangInfoG[A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.optionElmInfo(ai(lang))
  }

  override implicit def topInfo: LangInfoG[Unit] = new LangInfoG[Unit] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[Unit] = lang.topInfo
  }

  override def sumAssocRL[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new LangTerm[Either[A, Either[B, C]] => Either[Either[A, B], C]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.sumAssocRL(ai(lang), bi(lang), ci(lang))
    }

  override def getDouble: LangTerm[IO[Double]] = new LangTerm[IO[Double]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[IO[Double]] = lang.getDouble
  }

  override def recipD: LangTerm[Double => Double] = new LangTerm[Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.recipD
  }

  override def litD: Double => LangTerm[Double] = d => new LangTerm[Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[Double] = lang.litD(d)
  }

  override def curry[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new LangTerm[(((A, B)) => C) => A => B => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.curry(ai(lang), bi(lang), ci(lang))
    }

  override implicit def prodInfo[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new LangInfoG[(A, B)] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.prodInfo[A, B](ai(lang), bi(lang))
  }

  override def prodZroInfo[A, B]: LangInfoG[(A, B)] => LangInfoG[A] = x => new LangInfoG[A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[A] = lang.prodZroInfo(x(lang))
  }

  override def prodFstInfo[A, B]: LangInfoG[(A, B)] => LangInfoG[B] = x => new LangInfoG[B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[B] = lang.prodFstInfo(x(lang))
  }

  override def nil[A](implicit ai: LangInfoG[A]): LangTerm[List[A]] = new LangTerm[List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[List[A]] = lang.nil(ai(lang))
  }

  override def listMatch[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[List[A] => B => (A => List[A] => B) => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.listMatch(ai(lang), bi(lang))
    }

  override def App[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new LangTerm[(A => B) => A => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.App(ai(lang), bi(lang))
  }

  override def uncurry[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new LangTerm[(A => B => C) => ((A, B)) => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.uncurry(ai(lang), bi(lang), ci(lang))
    }

  override def optionMatch[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[Option[A] => B => (A => B) => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.optionMatch(ai(lang), bi(lang))
    }

  override def readerRet[E, A](implicit ei: LangInfoG[E], ai: LangInfoG[A]) = new LangTerm[A => Reader[E, A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.readerRet(ei(lang), ai(lang))
  }

  override def Let[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new LangTerm[A => (A => B) => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.Let(ai(lang), bi(lang))
  }

  override def minusD = new LangTerm[Double => Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.minusD
  }

  override def litB: Boolean => LangTerm[Boolean] = b => new LangTerm[Boolean] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.litB(b)
  }

  override def negD = new LangTerm[Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.negD
  }

  override implicit def botInfo = new LangInfoG[Nothing] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[Nothing] = lang.botInfo
  }

  override def ltD: LangTerm[Double => Double => Boolean] = new LangTerm[(Double) => (Double) => Boolean] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.ltD
  }

  override def IOInfo[A](implicit ai: LangInfoG[A]): LangInfoG[IO[A]] = new LangInfoG[IO[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[IO[A]] = lang.IOInfo(ai(lang))
  }

  override def IOElmInfo[A]: LangInfoG[IO[A]] => LangInfoG[A] = ioai => new LangInfoG[A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[A] = lang.IOElmInfo(ioai(lang))
  }

  override def sigD: LangTerm[Double => Double] = new LangTerm[Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.sigD
  }

  override implicit def boolInfo: LangInfoG[Boolean] = new LangInfoG[Boolean] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[Boolean] = lang.boolInfo
  }

  override implicit def doubleInfo: LangInfoG[Double] = new LangInfoG[Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[Double] = lang.doubleInfo
  }

  override def stateRet[S, A](implicit si: LangInfoG[S], ai: LangInfoG[A]) = new LangTerm[A => State[S, A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.stateRet[S, A](si(lang), ai(lang))
  }

  override def B[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new LangTerm[(B => C) => (A => B) => A => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.B(ai(lang), bi(lang), ci(lang))
    }

  override def listZip[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[List[A] => List[B] => List[(A, B)]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.listZip[A, B](ai(lang), bi(lang))
  }

  override def W[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new LangTerm[(A => A => B) => A => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.W[A, B](ai(lang), bi(lang))
  }

  override def divD = new LangTerm[Double => Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.divD
  }

  override def sumComm[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[Either[A, B] => Either[B, A]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.sumComm[A, B](ai(lang), bi(lang))
    }

  override def IORet[A](implicit ai: LangInfoG[A]) = new LangTerm[A => IO[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.IORet[A](ai(lang))
  }

  override def Y[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new LangTerm[((A => B) => A => B) => A => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.Y[A, B](ai(lang), bi(lang))
  }

  override def mkTop: LangTerm[Unit] = new LangTerm[Unit] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.mkTop
  }

  override def streamNil[A](implicit ai: LangInfoG[A]): LangTerm[Stream[A]] = new LangTerm[Stream[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[Stream[A]] = lang.streamNil(ai(lang))
  }

  override def streamCons[A](implicit ai: LangInfoG[A]) = new LangTerm[A => (Unit => Stream[A]) => Stream[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.streamCons[A](ai(lang))
  }

  override def sumAssocLR[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new LangTerm[Either[Either[A, B], C] => Either[A, Either[B, C]]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.sumAssocLR(ai(lang), bi(lang), ci(lang))
    }

  override def right[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): LangTerm[B => Either[A, B]] =
    new LangTerm[B => Either[A, B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.right[A, B](ai(lang), bi(lang))
  }

  override implicit def streamInfo[A](implicit ai: LangInfoG[A]): LangInfoG[Stream[A]] = new LangInfoG[Stream[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[Stream[A]] = lang.streamInfo(ai(lang))
  }

  override def streamElmInfo[A]: LangInfoG[Stream[A]] => LangInfoG[A] = x => new LangInfoG[A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[A] = lang.streamElmInfo(x(lang))
  }

  override def scanLeft[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[(B => A => B) => B => List[A] => List[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.scanLeft(ai(lang), bi(lang))
    }

  override def contRet[R, A](implicit ri: LangInfoG[R], ai: LangInfoG[A]) = new LangTerm[A => Cont[R, A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.contRet(ri(lang), ai(lang))
  }

  override implicit def sumInfo[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new LangInfoG[Either[A, B]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.sumInfo(ai(lang), bi(lang))
  }

  override def sumLeftInfo[A, B]: LangInfoG[Either[A, B]] => LangInfoG[A] = x => new LangInfoG[A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.sumLeftInfo(x(lang))
  }

  override def sumRightInfo[A, B]: LangInfoG[Either[A, B]] => LangInfoG[B] = x => new LangInfoG[B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.sumRightInfo(x(lang))
    }

  override def C[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new LangTerm[(A => B => C) => B => A => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.C(ai(lang), bi(lang), ci(lang))
    }

  override def S[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new LangTerm[(A => B => C) => (A => B) => A => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.S(ai(lang), bi(lang), ci(lang))
    }

  override implicit def aInfo[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new LangInfoG[A => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.aInfo(ai(lang), bi(lang))
  }

  override def domInfo[A, B]: LangInfoG[A => B] => LangInfoG[A] = x => new LangInfoG[A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[A] = lang.domInfo(x(lang))
  }

  override def rngInfo[A, B]: LangInfoG[A => B] => LangInfoG[B] = x => new LangInfoG[B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[B] = lang.rngInfo(x(lang))
  }

  override def reprInfo[A]: LangTerm[A] => LangInfoG[A] = x => new LangInfoG[A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[A] = lang.reprInfo(x(lang))
  }
}

object LangTermLang extends LangTermLang