package com.thoughtworks.DDF.Language

trait LangTermLang extends
  Lang[LangInfoG, LangTerm] with
  LangTermLangInfo[LangTerm] {

  override def K[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[A => B => A] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.K(ai(lang), bi(lang))
    }

  override def litB: Boolean => LangTerm[Boolean] = b =>
    new LangTerm[Boolean] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[Boolean] = lang.litB(b)
    }

  override def ite[A](implicit ai: LangInfoG[A]) = new LangTerm[Boolean => A => A => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.ite(ai(lang))
  }

  override def foldRight[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[(A => B => B) => B => List[A] => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.foldRight(ai(lang), bi(lang))
  }

  override def B[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new LangTerm[(B => C) => (A => B) => A => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.B(ai(lang), bi(lang), ci(lang))
    }

  override def listZip[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[List[A] => List[B] => List[(A, B)]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.listZip(ai(lang), bi(lang))
    }

  override def scanRight[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[(A => B => B) => B => List[A] => List[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.scanRight(ai(lang), bi(lang))
    }

  override def uncurry[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new LangTerm[(A => B => C) => ((A, B)) => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.uncurry(ai(lang), bi(lang), ci(lang))
    }

  override def sumComm[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[Either[A, B] => Either[B, A]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.sumComm(ai(lang), bi(lang))
    }

  override def sumAssocLR[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new LangTerm[Either[Either[A, B], C] => Either[A, Either[B, C]]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.sumAssocLR(ai(lang), bi(lang), ci(lang))
    }

  override def sumAssocRL[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new LangTerm[Either[A, Either[B, C]] => Either[Either[A, B], C]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.sumAssocRL(ai(lang), bi(lang), ci(lang))
    }

  override def sigD = new LangTerm[Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[Double => Double] = lang.sigD
  }

  override def plusD = new LangTerm[Double => Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.plusD
  }

  override def expD = new LangTerm[Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[Double => Double] = lang.expD
  }

  override def litD: Double => LangTerm[Double] = d => new LangTerm[Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[Double] = lang.litD(d)
  }

  override def I[A](implicit ai: LangInfoG[A]) = new LangTerm[A => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.I(ai(lang))
  }

  override def curry[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new LangTerm[(((A, B)) => C) => A => B => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.curry(ai(lang), bi(lang), ci(lang))
    }

  override def App[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[(A => B) => A => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.App(ai(lang), bi(lang))
    }

  override def W[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[(A => A => B) => A => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.W(ai(lang), bi(lang))
    }

  override def ltD = new LangTerm[Double => Double => Boolean] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.ltD
  }

  override def mkUnit: LangTerm[Unit] = new LangTerm[Unit] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[Unit] = lang.mkUnit
  }

  override def multD = new LangTerm[Double => Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.multD
  }

  override def app[A, B]: LangTerm[A => B] => LangTerm[A] => LangTerm[B] = f => x => new LangTerm[B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[B] = lang.app(f(lang))(x(lang))
  }

  override def S[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new LangTerm[(A => B => C) => (A => B) => A => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.S(ai(lang), bi(lang), ci(lang))
    }

  override def Y[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[((A => B) => (A => B)) => A => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.Y(ai(lang), bi(lang))
    }

  override def scanLeft[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[(B => A => B) => B => List[A] => List[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.scanLeft(ai(lang), bi(lang))
    }

  override def nil[A](implicit ai: LangInfoG[A]): LangTerm[List[A]] = new LangTerm[List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[List[A]] = lang.nil(ai(lang))
  }

  override def cons[A](implicit ai: LangInfoG[A]) = new LangTerm[A => List[A] => List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.cons(ai(lang))
  }

  override def listMatch[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[List[A] => B => (A => List[A] => B) => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.listMatch(ai(lang), bi(lang))
    }

  override def C[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new LangTerm[(A => B => C) => B => A => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.C(ai(lang), bi(lang), ci(lang))
    }

  override def reverse[A](implicit ai: LangInfoG[A]) = new LangTerm[List[A] => List[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.reverse(ai(lang))
  }

  override def listMap[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[(A => B) => List[A] => List[B]] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.listMap(ai(lang), bi(lang))
    }

  override def Let[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[A => (A => B) => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.Let(ai(lang), bi(lang))
    }

  override def none[A](implicit ai: LangInfoG[A]): LangTerm[Option[A]] = new LangTerm[Option[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Repr[Option[A]] = lang.none(ai(lang))
  }

  override def some[A](implicit ai: LangInfoG[A]) = new LangTerm[A => Option[A]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.some(ai(lang))
  }

  override def optionMatch[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[Option[A] => B => (A => B) => B] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.optionMatch(ai(lang), bi(lang))
    }

  override def divD = new LangTerm[Double => Double => Double] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.divD
  }

  override def foldLeft[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) =
    new LangTerm[(A => B => A) => A => List[B] => A] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.foldLeft(ai(lang), bi(lang))
    }

  override def mkProduct[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new LangTerm[A => B => (A, B)] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.mkProduct(ai(lang), bi(lang))
  }

  override def zeroth[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new LangTerm[((A, B)) => A] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.zeroth(ai(lang), bi(lang))
  }

  override def first[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new LangTerm[((A, B)) => B] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.first(ai(lang), bi(lang))
  }

  override def left[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new LangTerm[A => Either[A, B]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.left(ai(lang), bi(lang))
  }

  override def right[A, B](implicit ai: LangInfoG[A], bi: LangInfoG[B]) = new LangTerm[B => Either[A, B]] {
    override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) = lang.right(ai(lang), bi(lang))
  }

  override def sumMatch[A, B, C](implicit ai: LangInfoG[A], bi: LangInfoG[B], ci: LangInfoG[C]) =
    new LangTerm[Either[A, B] => (A => C) => (B => C) => C] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]) =
        lang.sumMatch(ai(lang), bi(lang), ci(lang))
    }

  override def reprInfo[A]: LangTerm[A] => LangInfoG[A] = x =>
    new LangInfoG[A] {
      override def apply[Info[_], Repr[_]](implicit lang: Lang[Info, Repr]): Info[A] = lang.reprInfo(x(lang))
    }
}

object LangTermLang extends LangTermLang
