package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Product.Prod
import com.thoughtworks.DDF.{FEMMatch, FEval, FEvalCase}

trait FEvalInterLang[G] extends FEvalProd[G] with InterLang[FEvalCase[G, ?], FEval[G, ?]] {

  override def scanLeft[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, (B => A => B) => B => List[A] => List[B]] {
      override val tm = aInfo(aInfo(bi, aInfo(ai, bi)), aInfo(bi, aInfo(listInfo(ai), listInfo(bi))))

      override val deriv = base.scanLeft(ai.lr, bi.lr)
    }

  override def sumAssocRL[A, B, C](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B], ci: FEvalCase[G, C]) =
    new FEval[G, Either[A, Either[B, C]] => Either[Either[A, B], C]] {
      override val tm = aInfo(sumInfo(ai, sumInfo(bi, ci)), sumInfo(sumInfo(ai, bi), ci))

      override val deriv = base.sumAssocRL(ai.lr, bi.lr, ci.lr)
    }

  override def none[A](implicit ai: FEvalCase[G, A]) =
    new FEval[G, Option[A]] {
      override val tm = optionInfo(ai)

      override val deriv = base.none(ai.lr)
    }

  override def some[A](implicit ai: FEvalCase[G, A]) =
    new FEval[G, A => Option[A]] {
      override val tm = aInfo(ai, optionInfo(ai))

      override val deriv = base.some(ai.lr)
    }

  override def optionMatch[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, Option[A] => B => (A => B) => B] {
      override val tm = aInfo(optionInfo(ai), aInfo(bi, aInfo(aInfo(ai, bi), bi)))

      override val deriv = base.optionMatch(ai.lr, bi.lr)
    }

  override def K[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) = new FEval[G, A => B => A] {
    override val tm = aInfo(ai, aInfo(bi, ai))

    override val deriv = base.K(ai.lr, bi.lr)
  }

  override def W[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, (A => A => B) => A => B] {
      override val tm = aInfo(aInfo(ai, aInfo(ai, bi)), aInfo(ai, bi))

      override val deriv = base.W(ai.lr, bi.lr)
    }

  override def listZip[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, List[A] => List[B] => List[(A, B)]] {
      override val tm = aInfo(listInfo(ai), aInfo(listInfo(bi), listInfo(prodInfo(ai, bi))))

      override val deriv = base.listZip(ai.lr, bi.lr)
    }

  def lfem[A] = new FEMMatch[G, List[A]] {
    override type ret = FEvalCase[G, A]
  }

  override implicit def listInfo[A](implicit ai: FEvalCase[G, A]): FEvalCase.Aux[G, List[A], List[ai.ret]] =
    new FEvalCase[G, List[A]] {
      override type ret = List[ai.ret]

      override def lr = base.listInfo(ai.lr)

      override val tm = lfem[A]

      override def tmr = ai
    }

  override def listElmInfo[A]: FEvalCase[G, List[A]] => FEvalCase[G, A] = _.get(lfem[A])

  override def ltD: FEval[G, Double => Double => Boolean] = ???

  override def C[A, B, C](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B], ci: FEvalCase[G, C]) =
    new FEval[G, (A => B => C) => B => A => C] {
      override val tm = aInfo(aInfo(ai, aInfo(bi, ci)), aInfo(bi, aInfo(ai, ci)))

      override val deriv = base.C(ai.lr, bi.lr, ci.lr)
    }

  override def listMap[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, (A => B) => List[A] => List[B]] {
      override val tm = aInfo(aInfo(ai, bi), aInfo(listInfo(ai), listInfo(bi)))

      override val deriv = base.listMap(ai.lr, bi.lr)
    }

  override def nil[A](implicit ai: FEvalCase[G, A]) =
    new FEval[G, List[A]] {
      override val tm = listInfo(ai)

      override val deriv = base.nil(ai.lr)
    }

  override def listMatch[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, List[A] => B => (A => List[A] => B) => B] {
      override val tm = aInfo(listInfo(ai), aInfo(bi, aInfo(aInfo(ai, aInfo(listInfo(ai), bi)), bi)))

      override val deriv = base.listMatch(ai.lr, bi.lr)
    }

  override def divD: FEval[G, Double => Double => Double] = ???

  override def sumMatch[A, B, C](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B], ci: FEvalCase[G, C]) =
    new FEval[G, Either[A, B] => (A => C) => (B => C) => C] {
      override val tm = aInfo(sumInfo(ai, bi), aInfo(aInfo(ai, ci), aInfo(aInfo(bi, ci), ci)))

      override val deriv: LangTerm[tm.ret] = base.sumMatch(ai.lr, bi.lr, ci.lr)
    }

  override def Y[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, ((A => B) => A => B) => A => B] {
      override val tm = aInfo(aInfo(aInfo(ai, bi), aInfo(ai, bi)), aInfo(ai, bi))

      override val deriv: LangTerm[tm.ret] = base.Y(ai.lr, bi.lr)
    }

  override implicit def botInfo: FEvalCase.Aux[G, Nothing, Nothing] =
    new FEvalCase[G, Nothing] {
      override type ret = Nothing

      override def lr: LangInfoG[Nothing] = base.botInfo

      override val tm = new FEMMatch[G, Nothing] {
        override type ret = Unit
      }

      override def tmr: tm.ret = ()
    }

  override def B[A, B, C](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B], ci: FEvalCase[G, C]) =
    new FEval[G, (B => C) => (A => B) => A => C] {
      override val tm = aInfo(aInfo(bi, ci), aInfo(aInfo(ai, bi), aInfo(ai, ci)))

      override val deriv: LangTerm[tm.ret] = base.B(ai.lr, bi.lr, ci.lr)
    }

  override def multD: FEval[G, Double => Double => Double] = ???

  override def sumAssocLR[A, B, C](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B], ci: FEvalCase[G, C]) =
    new FEval[G, Either[Either[A, B], C] => Either[A, Either[B, C]]] {
      override val tm = aInfo(sumInfo(sumInfo(ai, bi), ci), sumInfo(ai, sumInfo(bi, ci)))

      override val deriv = base.sumAssocLR(ai.lr, bi.lr, ci.lr)
    }

  override implicit def topInfo: FEvalCase.Aux[G, Unit, Unit] = new FEvalCase[G, Unit] {
    override type ret = Unit

    override val tm: FEMMatch.Aux[G, Unit, Unit] = new FEMMatch[G, Unit] {
      override type ret = Unit
    }

    override def tmr: tm.ret = ()

    override def lr: LangInfoG[Unit] = base.topInfo
  }

  override def ite[A](implicit ai: FEvalCase[G, A]) =
    new FEval[G, Boolean => A => A => A] {
      override val tm = aInfo(boolInfo, aInfo(ai, aInfo(ai, ai)))

      override val deriv = base.ite(ai.lr)
    }

  override def foldRight[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, (A => B => B) => B => List[A] => B] {
      override val tm = aInfo(aInfo(ai, aInfo(bi, bi)), aInfo(bi, aInfo(listInfo(ai), bi)))

      override val deriv = base.foldRight(ai.lr, bi.lr)
    }

  override def foldLeft[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, (A => B => A) => A => List[B] => A] {
      override val tm = aInfo(aInfo(ai, aInfo(bi, ai)), aInfo(ai, aInfo(listInfo(bi), ai)))

      override val deriv = base.foldLeft(ai.lr, bi.lr)
    }

  override def reverse[A](implicit ai: FEvalCase[G, A]) =
    new FEval[G, List[A] => List[A]] {
      override val tm = aInfo(listInfo(ai), listInfo(ai))

      override val deriv = base.reverse(ai.lr)
    }

  override def expD: FEval[G, (Double) => Double] = ???

  override def right[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, B => Either[A, B]] {
      override val tm = aInfo(bi, sumInfo(ai, bi))

      override val deriv = base.right(ai.lr, bi.lr)
    }

  def ofem[A] = new FEMMatch[G, Option[A]] {
    override type ret = FEvalCase[G, A]
  }

  override implicit def optionInfo[A](implicit ai: FEvalCase[G, A]): FEvalCase.Aux[G, Option[A], Option[ai.ret]] =
    new FEvalCase[G, Option[A]] {
      override type ret = Option[ai.ret]

      override val tm = ofem[A]

      override def tmr: tm.ret = ai

      override def lr: LangInfoG[Option[ai.ret]] = base.optionInfo(ai.lr)
    }

  override def optionElmInfo[A]: FEvalCase[G, Option[A]] => FEvalCase[G, A] = _.get(ofem[A])

  override def left[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, A => Either[A, B]] {
      override val tm = aInfo(ai, sumInfo(ai, bi))

      override val deriv = base.left(ai.lr, bi.lr)
    }

  override def S[A, B, C](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B], ci: FEvalCase[G, C]) =
    new FEval[G, (A => B => C) => (A => B) => A => C] {
      override val tm = aInfo(aInfo(ai, aInfo(bi, ci)), aInfo(aInfo(ai, bi), aInfo(ai, ci)))

      override val deriv: LangTerm[tm.ret] = base.S(ai.lr, bi.lr, ci.lr)
    }

  override def sumComm[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, Either[A, B] => Either[B, A]] {
      override val tm = aInfo(sumInfo(ai, bi), sumInfo(bi, ai))

      override val deriv: LangTerm[tm.ret] = base.sumComm(ai.lr, bi.lr)
    }

  override def scanRight[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, (A => B => B) => B => List[A] => List[B]] {
      override val tm = aInfo(aInfo(ai, aInfo(bi, bi)), aInfo(bi, aInfo(listInfo(ai), listInfo(bi))))

      override val deriv = base.scanRight(ai.lr, bi.lr)
    }

  override def cons[A](implicit ai: FEvalCase[G, A]) =
    new FEval[G, A => List[A] => List[A]] {
      override val tm = aInfo(ai, aInfo(listInfo(ai), listInfo(ai)))

      override val deriv = base.cons(ai.lr)
    }

  override def sigD: FEval[G, Double => Double] = ???

  override def plusD: FEval[G, Double => Double => Double] = ???

  override def Let[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, A => (A => B) => B] {
      override val tm = aInfo(ai, aInfo(aInfo(ai, bi), bi))
      override val deriv: LangTerm[tm.ret] = base.Let(ai.lr, bi.lr)
    }

  override def I[A](implicit ai: FEvalCase[G, A]): FEval[G, A => A] = new FEval[G, A => A] {
    override val tm = aInfo(ai, ai)

    override val deriv: LangTerm[tm.ret] = base.I(ai.lr)
  }

  override def app[A, B]: FEval[G, A => B] => FEval[G, A] => FEval[G, B] = f => x => new FEval[G, B] {
    val ai = domInfo(f.tm)

    val bi = rngInfo(f.tm)

    override val tm: FEvalCase.Aux[G, B, bi.ret] = bi

    override val deriv: LangTerm[tm.ret] = base.app(f.get(aInfo(ai, bi)))(x.get(ai))
  }

  def afem[A, B] = new FEMMatch[G, A => B] {
    override type ret = (FEvalCase[G, A], FEvalCase[G, B])
  }

  override implicit def aInfo[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]):
  FEvalCase.Aux[G, A => B, ai.ret => bi.ret] =
    new FEvalCase[G, A => B] {
      override type ret = ai.ret => bi.ret

      override val tm = afem[A, B]

      override def tmr: tm.ret = (ai, bi)

      override def lr: LangInfoG[ai.ret => bi.ret] = base.aInfo(ai.lr, bi.lr)
    }

  override def domInfo[A, B]: FEvalCase[G, A => B] => FEvalCase[G, A] = _.get(afem[A, B])._1

  override def rngInfo[A, B]: FEvalCase[G, A => B] => FEvalCase[G, B] = _.get(afem[A, B])._2

  override def mkTop: FEval[G, Unit] = new FEval[G, Unit] {
    override val tm = topInfo

    override val deriv: LangTerm[tm.ret] = base.mkTop
  }

  override def App[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]): FEval[G, (A => B) => A => B] =
    new FEval[G, (A => B) => A => B] {
      override val tm = aInfo(aInfo(ai, bi), aInfo(ai, bi))

      override val deriv: LangTerm[tm.ret] = base.App[ai.ret, bi.ret](ai.lr, bi.lr)
    }

  override def litD: Double => FEval[G, Double] = ???

  override implicit def doubleInfo: FEvalCase.Aux[G, Double, (Double, G)] = new FEvalCase[G, Double] {
    override type ret = (Double, G)

    override val tm: FEMMatch.Aux[G, Double, Unit] = new FEMMatch[G, Double] {
      override type ret = Unit
    }

    override def tmr: tm.ret = ()

    override def lr: LangInfoG[(Double, G)] = ???
  }

  override def litB: Boolean => FEval[G, Boolean] = b => new FEval[G, Boolean] {
    override val tm = boolInfo

    override val deriv: LangTerm[tm.ret] = base.litB(b)
  }

  override implicit def boolInfo: FEvalCase.Aux[G, Boolean, Boolean] = new FEvalCase[G, Boolean] {
    override type ret = Boolean

    override val tm: FEMMatch.Aux[G, Boolean, Unit] = new FEMMatch[G, Boolean] {
      override type ret = Unit
    }

    override def tmr: tm.ret = ()

    override def lr: LangInfoG[Boolean] = base.boolInfo
  }

  override def exfalso[A](implicit ai: FEvalCase[G, A]) =
    new FEval[G, Nothing => A] {
      override val tm = aInfo[Nothing, A](botInfo, ai)

      override val deriv = base.exfalso(ai.lr)
    }

  def sfem[A, B] = new FEMMatch[G, Either[A, B]] {
    override type ret = (FEvalCase[G, A], FEvalCase[G, B])
  }

  override implicit def sumInfo[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]):
  FEvalCase.Aux[G, Either[A, B], Either[ai.ret, bi.ret]] = new FEvalCase[G, Either[A, B]] {
    override type ret = Either[ai.ret, bi.ret]

    override def lr: LangInfoG[Either[ai.ret, bi.ret]] = base.sumInfo(ai.lr, bi.lr)

    override val tm = sfem[A, B]

    override def tmr: tm.ret = (ai, bi)
  }

  override def sumLeftInfo[A, B]: FEvalCase[G, Either[A, B]] => FEvalCase[G, A] = _.get(sfem[A, B])._1

  override def sumRightInfo[A, B]: FEvalCase[G, Either[A, B]] => FEvalCase[G, B] = _.get(sfem[A, B])._2

  override def reprInfo[A]: FEval[G, A] => FEvalCase[G, A] = _.tm

  override def putDouble: FEval[G, Double => IO[Unit]] = ???

  override def IOBind[A, B](implicit ai: FEvalCase[G, A], bi: FEvalCase[G, B]) =
    new FEval[G, IO[A] => (A => IO[B]) => IO[B]] {
      override val tm = aInfo(IOInfo(ai), aInfo(aInfo(ai, IOInfo(bi)), IOInfo(bi)))

      override val deriv = base.IOBind(ai.lr, bi.lr)
    }

  override def IORet[A](implicit ai: FEvalCase[G, A]): FEval[G, A => IO[A]] =
    new FEval[G, A => IO[A]] {
      override val tm = aInfo(ai, IOInfo(ai))

      override val deriv: LangTerm[tm.ret] = base.IORet(ai.lr)
    }

  override def getDouble: FEval[G, IO[Double]] = ???

  def iofem[A] = new FEMMatch[G, IO[A]] {
    override type ret = FEvalCase[G, A]
  }

  override def IOInfo[A](implicit ai: FEvalCase[G, A]): FEvalCase.Aux[G, IO[A], IO[ai.ret]] =
    new FEvalCase[G, IO[A]] {
      override def lr: LangInfoG[IO[ai.ret]] = base.IOInfo(ai.lr)

      override type ret = IO[ai.ret]

      override val tm = iofem[A]

      override def tmr: tm.ret = ai
    }

  override def IOElmInfo[A]: FEvalCase[G, IO[A]] => FEvalCase[G, A] = _.get(iofem[A])
}

object FEvalInterLang {
  implicit def apply[G]: FEvalInterLang[G] = new FEvalInterLang[G] { }
}