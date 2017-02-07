package com.thoughtworks.DDF.Language
import com.thoughtworks.DDF.RecursiveInfoMatch._

trait LangTermLang extends Lang[LangInfoG, LangTerm] {
  override implicit def botInfo = new LangInfoG[Nothing] with BotRI[LangInfoGMatch] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[Nothing] = lang.botInfo
  }

  override def ite[A](implicit ai: LangInfoG[A]) = new RawLangTerm[Boolean => A => A => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.ite[A](ai(lang))
  }.convert

  override def K[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[A => B => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.K[A, B](ai(lang), bi(lang))
  }.convert

  override def multD = new RawLangTerm[Double => Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.multD
  }.convert

  override def some[A](implicit ai: LangInfoG[A]) = new RawLangTerm[A => Option[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.some[A](ai(lang))
  }.convert

  override def reverse[A](implicit ai: LangInfoG[A]) = new RawLangTerm[List[A] => List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.reverse[A](ai(lang))
  }.convert

  override def foldLeft[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[(A => B => A) => A => List[B] => A] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.foldLeft[A, B](ai(lang), bi(lang))
    }.convert

  override def cons[A](implicit ai: LangInfoG[A]) = new RawLangTerm[A => List[A] => List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.cons[A](ai(lang))
  }.convert

  override def expD = new RawLangTerm[Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.expD
  }.convert

  override def exceptBind[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[Except[A, B] => (B => Except[A, C]) => Except[A, C]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.exceptBind(ai(lang), bi(lang), ci(lang))
    }.convert

  override def mkProd[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[A => B => (A, B)] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.mkProd(ai(lang), bi(lang))
  }.convert

  override def app[A, B]: LangTerm[A => B] => LangTerm[A] => LangTerm[B] = f => x => new LangTerm[B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[B] = lang.app(f(lang))(x(lang))

    override def info: LangInfoG[B] = rngInfo(f.info)
  }

  override implicit def listInfo[A](implicit ai: LangInfoG[A]) =
    new LangInfoG[List[A]] with ListRI[LangInfoGMatch, LangInfoG, A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[List[A]] = lang.listInfo(ai(lang))

    override def tmr: tm.ret = ai
  }

  override def listElmInfo[A]: LangInfoG[List[A]] => LangInfoG[A] = _.get(LM[LangInfoGMatch, LangInfoG, A])

  override def impossible = new LangTerm[Unit => Nothing] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.impossible

    override def info: LangInfoG[Unit => Nothing] = aInfo[Unit, Nothing]
  }

  override def contBind[R, A, B](implicit ri: LangInfoG[R], ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[Cont[R, A] => (A => Cont[R, B]) => Cont[R, B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.contBind(ri(lang), ai(lang), bi(lang))
    }.convert

  override def putDouble = new RawLangTerm[Double => IO[Unit]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.putDouble
  }.convert

  override def I[A](implicit ai: LangInfoG[A]) = new RawLangTerm[A => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.I[A](ai(lang))
  }.convert

  override def listMap[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[(A => B) => List[A] => List[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.listMap(ai(lang), bi(lang))
    }.convert

  override def scanRight[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[(A => B => B) => B => List[A] => List[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.scanRight(ai(lang), bi(lang))
    }.convert

  override def litString: String => LangTerm[String] = str => new RawLangTerm[String] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[String] = lang.litString(str)
  }.convert

  override def imfalso[A](implicit ai: LangInfoG[A]) = new RawLangTerm[Unit => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.imfalso[A](ai(lang))
  }.convert

  override def sumMatch[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[Either[A, B] => (A => C) => (B => C) => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.sumMatch(ai(lang), bi(lang), ci(lang))
    }.convert

  override def left[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[A => Either[A, B]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.left[A, B](ai(lang), bi(lang))
  }.convert

  override implicit def stringInfo: LangInfoG[String] = new LangInfoG[String] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[String] = lang.stringInfo

    override val tm: LangInfoGMatch.Aux[String, Unit] = new LangInfoGMatch[String] {
      override type ret = Unit
    }

    override def tmr: tm.ret = ()
  }

  override def stateBind[S, A, B](implicit si: LangInfoG[S], ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[State[S, A] => (A => State[S, B]) => State[S, B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.stateBind(si(lang), ai(lang), bi(lang))
    }.convert

  override def exfalso[A](implicit ai: LangInfoG[A]) = new LangTerm[Nothing => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.exfalso[A](ai(lang))

    override def info: LangInfoG[Nothing => A] = aInfo[Nothing, A]
  }

  override def zro[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[((A, B)) => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.zro(ai(lang), bi(lang))
  }.convert

  override def plusD = new RawLangTerm[Double => Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.plusD
  }.convert

  override def streamMatch[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[Stream[A] => B => (A => Stream[A] => B) => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.streamMatch(ai(lang), bi(lang))
    }.convert

  override def ><[A, B, C, D](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C], di: LangInfoG[D]) =
    new RawLangTerm[(A => C) => (B => D) => ((A, B)) => (C, D)] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.`><`(ai(lang), bi(lang), ci(lang), di(lang))
    }.convert

  override def readerBind[E, A, B](implicit ei: LangInfoG[E], ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[Reader[E, A] => (A => Reader[E, B]) => Reader[E, B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.readerBind(ei(lang), ai(lang), bi(lang))
    }.convert

  override def none[A](implicit ai: LangInfoG[A]): LangTerm[Option[A]] = new RawLangTerm[Option[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[Option[A]] = lang.none[A](ai(lang))
  }.convert

  override def foldRight[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[(A => B => B) => B => List[A] => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.foldRight[A, B](ai(lang), bi(lang))
    }.convert

  override def IOBind[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[IO[A] => (A => IO[B]) => IO[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.IOBind(ai(lang), bi(lang))
    }.convert

  override def fst[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[((A, B)) => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.fst(ai(lang), bi(lang))
  }.convert

  override implicit def optionInfo[A](implicit ai: LangInfoG[A]) =
    new LangInfoG[Option[A]] with OptionRI[LangInfoGMatch, LangInfoG, A] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.optionInfo(ai(lang))

      override def tmr: tm.ret = ai
  }

  override def optionElmInfo[A]: LangInfoG[Option[A]] => LangInfoG[A] = _.get(OM[LangInfoGMatch, LangInfoG, A])

  override implicit def topInfo: LangInfoG[Unit] = new LangInfoG[Unit] with TopRI[LangInfoGMatch] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[Unit] = lang.topInfo
  }

  override def sumAssocRL[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[Either[A, Either[B, C]] => Either[Either[A, B], C]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.sumAssocRL(ai(lang), bi(lang), ci(lang))
    }.convert

  override def getDouble: LangTerm[IO[Double]] = new RawLangTerm[IO[Double]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[IO[Double]] = lang.getDouble
  }.convert

  override def recipD: LangTerm[Double => Double] = new RawLangTerm[Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.recipD
  }.convert

  override def litD: Double => LangTerm[Double] = d => new RawLangTerm[Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[Double] = lang.litD(d)
  }.convert

  override def curry[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[(((A, B)) => C) => A => B => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.curry(ai(lang), bi(lang), ci(lang))
    }.convert

  override implicit def prodInfo[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangInfoG[(A, B)] with ProductRI[LangInfoGMatch, LangInfoG, A, B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.prodInfo[A, B](ai(lang), bi(lang))

      override def tmr: tm.ret = (ai, bi)
    }

  override def prodZroInfo[A, B]: LangInfoG[(A, B)] => LangInfoG[A] = _.get(PM[LangInfoGMatch, LangInfoG, A, B])._1

  override def prodFstInfo[A, B]: LangInfoG[(A, B)] => LangInfoG[B] = _.get(PM[LangInfoGMatch, LangInfoG, A, B])._2

  override def nil[A](implicit ai: LangInfoG[A]): LangTerm[List[A]] = new RawLangTerm[List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[List[A]] = lang.nil(ai(lang))
  }.convert

  override def listMatch[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[List[A] => B => (A => List[A] => B) => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.listMatch(ai(lang), bi(lang))
    }.convert

  override def App[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[(A => B) => A => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.App(ai(lang), bi(lang))
  }.convert

  override def uncurry[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[(A => B => C) => ((A, B)) => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.uncurry(ai(lang), bi(lang), ci(lang))
    }.convert

  override def optionMatch[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[Option[A] => B => (A => B) => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.optionMatch(ai(lang), bi(lang))
    }.convert

  override def readerRet[E, A](implicit ei: LangInfoG[E], ai: LangInfoG[A]) = new RawLangTerm[A => Reader[E, A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.readerRet(ei(lang), ai(lang))
  }.convert

  override def Let[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[A => (A => B) => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.Let(ai(lang), bi(lang))
  }.convert

  override def minusD = new RawLangTerm[Double => Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.minusD
  }.convert

  override def litB: Boolean => LangTerm[Boolean] = b => new RawLangTerm[Boolean] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.litB(b)
  }.convert

  override def negD = new RawLangTerm[Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.negD
  }.convert

  override def ltD: LangTerm[Double => Double => Boolean] = new RawLangTerm[(Double) => (Double) => Boolean] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.ltD
  }.convert

  override implicit def IOInfo[A](implicit ai: LangInfoG[A]): LangInfoG[IO[A]] =
    new LangInfoG[IO[A]] with IORI[LangInfoGMatch, LangInfoG, A] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[IO[A]] = lang.IOInfo(ai(lang))

      override def tmr: tm.ret = ai
  }

  override def IOElmInfo[A]: LangInfoG[IO[A]] => LangInfoG[A] = _.get(IOM[LangInfoGMatch, LangInfoG, A])

  override def sigD: LangTerm[Double => Double] = new RawLangTerm[Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.sigD
  }.convert

  override implicit def boolInfo: LangInfoG[Boolean] = new LangInfoG[Boolean] with BoolRI[LangInfoGMatch] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[Boolean] = lang.boolInfo
  }

  override implicit def doubleInfo: LangInfoG[Double] = new LangInfoG[Double] with DoubleRI[LangInfoGMatch] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[Double] = lang.doubleInfo
  }

  override def stateRet[S, A](implicit si: LangInfoG[S], ai: LangInfoG[A]) = new RawLangTerm[A => State[S, A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.stateRet[S, A](si(lang), ai(lang))
  }.convert

  override def B[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[(B => C) => (A => B) => A => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.B(ai(lang), bi(lang), ci(lang))
    }.convert

  override def listZip[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[List[A] => List[B] => List[(A, B)]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.listZip[A, B](ai(lang), bi(lang))
  }.convert

  override def W[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[(A => A => B) => A => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.W[A, B](ai(lang), bi(lang))
  }.convert

  override def divD = new RawLangTerm[Double => Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.divD
  }.convert

  override def sumComm[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[Either[A, B] => Either[B, A]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.sumComm[A, B](ai(lang), bi(lang))
    }.convert

  override def IORet[A](implicit ai: LangInfoG[A]) = new RawLangTerm[A => IO[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.IORet[A](ai(lang))
  }.convert

  override def Y[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new RawLangTerm[((A => B) => A => B) => A => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.Y[A, B](ai(lang), bi(lang))
  }.convert

  override def mkTop: LangTerm[Unit] = new RawLangTerm[Unit] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.mkTop
  }.convert

  override def streamNil[A](implicit ai: LangInfoG[A]): LangTerm[Stream[A]] = new RawLangTerm[Stream[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[Stream[A]] = lang.streamNil(ai(lang))
  }.convert

  override def streamCons[A](implicit ai: LangInfoG[A]) = new RawLangTerm[A => (Unit => Stream[A]) => Stream[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.streamCons[A](ai(lang))
  }.convert

  override def sumAssocLR[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[Either[Either[A, B], C] => Either[A, Either[B, C]]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.sumAssocLR(ai(lang), bi(lang), ci(lang))
    }.convert

  override def right[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]): LangTerm[B => Either[A, B]] =
    new RawLangTerm[B => Either[A, B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.right[A, B](ai(lang), bi(lang))
  }.convert

  override implicit def streamInfo[A](implicit ai: LangInfoG[A]): LangInfoG[Stream[A]] =
    new LangInfoG[Stream[A]] with StreamRI[LangInfoGMatch, LangInfoG, A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[Stream[A]] = lang.streamInfo(ai(lang))

    override def tmr: tm.ret = ai
  }

  override def streamElmInfo[A]: LangInfoG[Stream[A]] => LangInfoG[A] = _.get(StM[LangInfoGMatch, LangInfoG, A])

  override def scanLeft[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new RawLangTerm[(B => A => B) => B => List[A] => List[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.scanLeft(ai(lang), bi(lang))
    }.convert

  override def contRet[R, A](implicit ri: LangInfoG[R], ai: LangInfoG[A]) = new RawLangTerm[A => Cont[R, A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.contRet(ri(lang), ai(lang))
  }.convert

  override implicit def sumInfo[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangInfoG[Either[A, B]] with SumRI[LangInfoGMatch, LangInfoG, A, B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.sumInfo(ai(lang), bi(lang))

    override def tmr: tm.ret = (ai, bi)
  }

  override def sumLeftInfo[A, B]: LangInfoG[Either[A, B]] => LangInfoG[A] =
    _.get(SM[LangInfoGMatch, LangInfoG, A, B])._1

  override def sumRightInfo[A, B]: LangInfoG[Either[A, B]] => LangInfoG[B] =
    _.get(SM[LangInfoGMatch, LangInfoG, A, B])._2

  override def C[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[(A => B => C) => B => A => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.C(ai(lang), bi(lang), ci(lang))
    }.convert

  override def S[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new RawLangTerm[(A => B => C) => (A => B) => A => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.S(ai(lang), bi(lang), ci(lang))
    }.convert

  override implicit def aInfo[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangInfoG[A => B] with ArrowRI[LangInfoGMatch, LangInfoG, A, B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.aInfo(ai(lang), bi(lang))

    override def tmr: tm.ret = (ai, bi)
  }

  override def domInfo[A, B]: LangInfoG[A => B] => LangInfoG[A] = _.get(AM[LangInfoGMatch, LangInfoG, A, B])._1

  override def rngInfo[A, B]: LangInfoG[A => B] => LangInfoG[B] = _.get(AM[LangInfoGMatch, LangInfoG, A, B])._2

  override def reprInfo[A]: LangTerm[A] => LangInfoG[A] = _.info
}

object LangTermLang extends LangTermLang