package com.thoughtworks.DDF.Language

trait InterLangTermInterLang extends
  InterLang[InterLangInfoG, InterLangTerm] with
  LangTermLangInfo[InterLangTerm] {

  override def K[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangTerm[A => B => A] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.K(ai(lang), bi(lang))
    }

  override def litB: Boolean => InterLangTerm[Boolean] = b =>
    new InterLangTerm[Boolean] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Repr[Boolean] = lang.litB(b)
    }

  override def ite[A](implicit ai: InterLangInfoG[A]) = new InterLangTerm[Boolean => A => A => A] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.ite(ai(lang))
  }

  override def foldRight[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangTerm[(A => B => B) => B => List[A] => B] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.foldRight(ai(lang), bi(lang))
  }

  override def B[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]) =
    new InterLangTerm[(B => C) => (A => B) => A => C] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.B(ai(lang), bi(lang), ci(lang))
    }

  override def listZip[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangTerm[List[A] => List[B] => List[(A, B)]] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.listZip(ai(lang), bi(lang))
    }

  override def scanRight[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangTerm[(A => B => B) => B => List[A] => List[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.scanRight(ai(lang), bi(lang))
    }

  override def uncurry[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]) =
    new InterLangTerm[(A => B => C) => ((A, B)) => C] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.uncurry(ai(lang), bi(lang), ci(lang))
    }

  override def sumComm[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangTerm[Either[A, B] => Either[B, A]] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.sumComm(ai(lang), bi(lang))
    }

  override def sumAssocLR[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]) =
    new InterLangTerm[Either[Either[A, B], C] => Either[A, Either[B, C]]] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) =
        lang.sumAssocLR(ai(lang), bi(lang), ci(lang))
    }

  override def sumAssocRL[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]) =
    new InterLangTerm[Either[A, Either[B, C]] => Either[Either[A, B], C]] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) =
        lang.sumAssocRL(ai(lang), bi(lang), ci(lang))
    }

  override def sigD = new InterLangTerm[Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Repr[Double => Double] = lang.sigD
  }

  override def plusD = new InterLangTerm[Double => Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.plusD
  }

  override def expD = new InterLangTerm[Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Repr[Double => Double] = lang.expD
  }

  override def litD: Double => InterLangTerm[Double] = d => new InterLangTerm[Double] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Repr[Double] = lang.litD(d)
  }

  override def I[A](implicit ai: InterLangInfoG[A]) = new InterLangTerm[A => A] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.I(ai(lang))
  }

  override def curry[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]) =
    new InterLangTerm[(((A, B)) => C) => A => B => C] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.curry(ai(lang), bi(lang), ci(lang))
    }

  override def App[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangTerm[(A => B) => A => B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.App(ai(lang), bi(lang))
    }

  override def W[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangTerm[(A => A => B) => A => B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.W(ai(lang), bi(lang))
    }

  override def ltD = new InterLangTerm[Double => Double => Boolean] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.ltD
  }

  override def mkTop: InterLangTerm[Unit] = new InterLangTerm[Unit] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Repr[Unit] = lang.mkTop
  }

  override def multD = new InterLangTerm[Double => Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.multD
  }

  override def app[A, B]: InterLangTerm[A => B] => InterLangTerm[A] => InterLangTerm[B] = f => x => new InterLangTerm[B] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Repr[B] = lang.app(f(lang))(x(lang))
  }

  override def S[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]) =
    new InterLangTerm[(A => B => C) => (A => B) => A => C] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.S(ai(lang), bi(lang), ci(lang))
    }

  override def Y[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangTerm[((A => B) => (A => B)) => A => B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.Y(ai(lang), bi(lang))
    }

  override def scanLeft[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangTerm[(B => A => B) => B => List[A] => List[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.scanLeft(ai(lang), bi(lang))
    }

  override def nil[A](implicit ai: InterLangInfoG[A]): InterLangTerm[List[A]] = new InterLangTerm[List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Repr[List[A]] = lang.nil(ai(lang))
  }

  override def cons[A](implicit ai: InterLangInfoG[A]) = new InterLangTerm[A => List[A] => List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.cons(ai(lang))
  }

  override def listMatch[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangTerm[List[A] => B => (A => List[A] => B) => B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.listMatch(ai(lang), bi(lang))
    }

  override def C[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]) =
    new InterLangTerm[(A => B => C) => B => A => C] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.C(ai(lang), bi(lang), ci(lang))
    }

  override def reverse[A](implicit ai: InterLangInfoG[A]) = new InterLangTerm[List[A] => List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.reverse(ai(lang))
  }

  override def listMap[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangTerm[(A => B) => List[A] => List[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.listMap(ai(lang), bi(lang))
    }

  override def Let[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangTerm[A => (A => B) => B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.Let(ai(lang), bi(lang))
    }

  override def none[A](implicit ai: InterLangInfoG[A]): InterLangTerm[Option[A]] = new InterLangTerm[Option[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Repr[Option[A]] = lang.none(ai(lang))
  }

  override def some[A](implicit ai: InterLangInfoG[A]) = new InterLangTerm[A => Option[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.some(ai(lang))
  }

  override def optionMatch[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangTerm[Option[A] => B => (A => B) => B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.optionMatch(ai(lang), bi(lang))
    }

  override def divD = new InterLangTerm[Double => Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.divD
  }

  override def foldLeft[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangTerm[(A => B => A) => A => List[B] => A] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.foldLeft(ai(lang), bi(lang))
    }

  override def mkProd[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) = new InterLangTerm[A => B => (A, B)] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.mkProd(ai(lang), bi(lang))
  }

  override def zro[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) = new InterLangTerm[((A, B)) => A] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.zro(ai(lang), bi(lang))
  }

  override def fst[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) = new InterLangTerm[((A, B)) => B] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.fst(ai(lang), bi(lang))
  }

  override def left[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) = new InterLangTerm[A => Either[A, B]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.left(ai(lang), bi(lang))
  }

  override def right[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) = new InterLangTerm[B => Either[A, B]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.right(ai(lang), bi(lang))
  }

  override def sumMatch[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]) =
    new InterLangTerm[Either[A, B] => (A => C) => (B => C) => C] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) =
        lang.sumMatch(ai(lang), bi(lang), ci(lang))
    }

  override def exfalso[A](implicit ai: InterLangInfoG[A]): InterLangTerm[Nothing => A] =
    new InterLangTerm[Nothing => A] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.exfalso(ai(lang))
    }

  override implicit def botInfo: InterLangInfoG[Nothing] = new InterLangInfoG[Nothing] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Nothing] = lang.botInfo
  }

  override def putDouble = new InterLangTerm[Double => IO[Unit]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.putDouble
  }

  override def IOBind[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangTerm[IO[A] => (A => IO[B]) => IO[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.IOBind(ai(lang), bi(lang))
    }

  override def IORet[A](implicit ai: InterLangInfoG[A]) = new InterLangTerm[A => IO[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.IORet(ai(lang))
  }

  override def getDouble = new InterLangTerm[IO[Double]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.getDouble
  }

  override def streamCons[A](implicit ai: InterLangInfoG[A]) =
    new InterLangTerm[(A) => ((Unit) => Stream[A]) => Stream[A]] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.streamCons(ai(lang))
    }

  override def streamNil[A](implicit ai: InterLangInfoG[A]) = new InterLangTerm[Stream[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.streamNil(ai(lang))
  }

  override def streamMatch[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]) =
    new InterLangTerm[Stream[A] => B => (A => Stream[A] => B) => B] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]) = lang.streamMatch(ai(lang), bi(lang))
    }

  override def reprInfo[A]: InterLangTerm[A] => InterLangInfoG[A] = a =>
    new InterLangInfoG[A] {
      override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[A] = lang.reprInfo(a(lang))
    }
}

object InterLangTermInterLang extends InterLangTermInterLang
