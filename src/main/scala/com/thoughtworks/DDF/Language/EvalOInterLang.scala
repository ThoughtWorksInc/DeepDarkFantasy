package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.{EvalO, EvalOMatch, NoInfo}

trait EvalOInterLang extends InterLang[InterLangInfoG, EvalO] with LangTermLangInfo[EvalO] {
  val ltl = InterLangTermInterLang

  def eval[X]: EvalO[X] => X = x => x.l.apply[NoInfo, Lambda[X => X]](EvalMInterLang)

  def aeval[A, B]: InterLangTerm[A => B] => (EvalO[A] => EvalO[B]) => EvalO[A => B] = la => f =>
    new EvalO[A => B] {
      override def l: InterLangTerm[A => B] = la

      override def tmr: tm.ret = f

      override val tm: EvalOMatch.Aux[A => B, EvalO[A] => EvalO[B]] = AEM[A, B]
    }

  override def B[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]):
  EvalO[(B => C) => (A => B) => A => C] =
    aeval(ltl.B[A, B, C])(f => aeval(ltl.B_[A, B, C](f.l))(g => aeval(ltl.B__(f.l)(g.l))(x => app(f)(app(g)(x)))))

  def AEM[A, B]: EvalOMatch.Aux[A => B, EvalO[A] => EvalO[B]] = new EvalOMatch[A => B] {
    override type ret = EvalO[A] => EvalO[B]
  }

  override def app[A, B]: EvalO[A => B] => EvalO[A] => EvalO[B] = f => f.get(AEM[A, B])

  override def scanRight[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]):
  EvalO[(A => B => B) => B => List[A] => List[B]] =
    aeval(ltl.scanRight[A, B])(f => aeval(ltl.scanRight_(f.l))(z => aeval(ltl.scanRight__(f.l)(z.l))(l =>
      leval(getList(l).scanRight(z)((x, y) => app(app(f)(x))(y))))))

  override def divD: EvalO[Double => Double => Double] =
    aeval(ltl.divD)(l => aeval(ltl.divD_(l.l))(r => litD(eval(l) / eval(r))))

  override def ltD: EvalO[Double => Double => Boolean] =
    aeval(ltl.ltD)(l => aeval(ltl.ltD_(l.l))(r => litB(eval(l) < eval(r))))

  override def scanLeft[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]):
  EvalO[(B => A => B) => B => List[A] => List[B]] =
    aeval(ltl.scanLeft[A, B])(f => aeval(ltl.scanLeft_(f.l))(z => aeval(ltl.scanLeft__(f.l)(z.l))(l =>
      leval(getList(l).scanLeft(z)((x, y) => app(app(f)(x))(y))))))

  override def listZip[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[List[A] => List[B] => List[(A, B)]] =
    aeval(ltl.listZip[A, B])(l => aeval(ltl.listZip_[A, B](l.l))(r =>
      leval(getList(l).zip(getList(r)).map(p => mkProduct__(p._1)(p._2)))))

  override def foldRight[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[(A => B => B) => B => List[A] => B] =
    aeval(ltl.foldRight[A, B])(f => aeval(ltl.foldRight_(f.l))(z => aeval(ltl.foldRight__(f.l)(z.l))(l =>
      getList(l).foldRight(z)((x, y) => app(app(f)(x))(y)))))

  def otm[A]: EvalOMatch.Aux[Option[A], Option[EvalO[A]]] = new EvalOMatch[Option[A]] {
    override type ret = Option[EvalO[A]]
  }

  override def none[A](implicit ai: InterLangInfoG[A]): EvalO[Option[A]] = new EvalO[Option[A]] {
    override def l: InterLangTerm[Option[A]] = ltl.none[A]

    override def tmr: tm.ret = None

    override val tm = otm[A]
  }

  override def some[A](implicit ai: InterLangInfoG[A]): EvalO[A => Option[A]] =
    aeval(ltl.some[A])(a => new EvalO[Option[A]] {
      override def l: InterLangTerm[Option[A]] = ltl.some_(a.l)

      override def tmr: tm.ret = Some(a)

      override val tm = otm[A]
    })

  override def optionMatch[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[Option[A] => B => (A => B) => B] =
    aeval(ltl.optionMatch[A, B])(_.get(otm[A]) match {
      case None => K[B, A => B]
      case Some(x) => K_[(A => B) => B, B](Let_[A, B](x))
    })

  override def I[A](implicit ai: InterLangInfoG[A]): EvalO[A => A] = aeval(ltl.I[A])(identity[EvalO[A]])

  override def foldLeft[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[(A => B => A) => A => List[B] => A] =
    aeval(ltl.foldLeft[A, B])(f => aeval(ltl.foldLeft_(f.l))(z => aeval(ltl.foldLeft__(f.l)(z.l))(l =>
      getList(l).foldLeft(z)((x, y) => app(app(f)(x))(y)))))

  override def listMap[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[(A => B) => List[A] => List[B]] =
    aeval(ltl.listMap[A, B])(f => aeval(ltl.listMap_(f.l))(l => leval(getList(l).map(app(f)))))

  override def reverse[A](implicit ai: InterLangInfoG[A]): EvalO[List[A] => List[A]] =
    aeval(ltl.reverse[A])(l => leval(getList(l).reverse))

  def ltm[A]: EvalOMatch.Aux[List[A], Option[(EvalO[A], EvalO[List[A]])]] = new EvalOMatch[List[A]] {
    override type ret = Option[(EvalO[A], EvalO[List[A]])]
  }

  def getList[A]: EvalO[List[A]] => List[EvalO[A]] = _.get(ltm[A]) match {
    case None => Nil
    case Some((h, t)) => h :: getList(t)
  }

  def leval[A](l: List[EvalO[A]])(implicit ai: InterLangInfoG[A]): EvalO[List[A]] = l match {
    case Nil => nil[A]
    case h :: t => cons__(h)(leval(t))
  }

  override def nil[A](implicit ai: InterLangInfoG[A]): EvalO[List[A]] = new EvalO[List[A]] {
    override def l: InterLangTerm[List[A]] = ltl.nil[A]

    override def tmr: tm.ret = None

    override val tm = ltm[A]
  }

  override def cons[A](implicit ai: InterLangInfoG[A]): EvalO[A => List[A] => List[A]] =
    aeval(ltl.cons[A])(h => aeval(ltl.cons_(h.l))(t => new EvalO[List[A]] {
      override def l: InterLangTerm[List[A]] = ltl.cons__(h.l)(t.l)

      override def tmr: tm.ret = Some((h, t))

      override val tm = ltm[A]
    }))

  override def listMatch[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]):
  EvalO[List[A] => B => (A => List[A] => B) => B] = aeval(ltl.listMatch[A, B])(_.get(ltm[A]) match {
    case None => K[B, (A => List[A] => B)]
    case Some((h, t)) => K_(B__(Let_[List[A], B](t))(Let_[A, List[A] => B](h)))
  })

  override def C[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]):
  EvalO[(A => B => C) => B => A => C] =
    aeval(ltl.C[A, B, C])(f => aeval(ltl.C_(f.l))(b => aeval(ltl.C__(f.l)(b.l))(a => app(app(f)(a))(b))))

  override def App[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[(A => B) => A => B] = I[A => B]

  override def uncurry[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]):
  EvalO[(A => B => C) => ((A, B)) => C] = aeval(ltl.uncurry[A, B, C])(f =>
    aeval(ltl.uncurry_(f.l))(p => app(app(f)(zro_(p)))(fst_(p))))

  override def plusD: EvalO[Double => Double => Double] =
    aeval(ltl.plusD)(l => aeval(ltl.plusD_(l.l))(r => litD(eval(l) + eval(r))))

  override def mkUnit: EvalO[Unit] = new EvalO[Unit] {
    override def l: InterLangTerm[Unit] = ltl.mkUnit

    override def tmr: tm.ret = ()

    override val tm: EvalOMatch.Aux[Unit, Unit] = new EvalOMatch[Unit] {
      override type ret = Unit
    }
  }

  override def sigD: EvalO[Double => Double] = aeval(ltl.sigD)(x => litD(1 / (1 + Math.exp(-eval(x)))))

  override def multD: EvalO[Double => Double => Double] =
    aeval(ltl.multD)(l => aeval(ltl.multD_(l.l))(r => litD(eval(l) * eval(r))))

  def ptm[A, B]: EvalOMatch.Aux[(A, B), (EvalO[A], EvalO[B])] = new EvalOMatch[(A, B)] {
    override type ret = (EvalO[A], EvalO[B])
  }

  override def mkProduct[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[A => B => (A, B)] =
    aeval(ltl.mkProduct[A, B])(x => aeval(ltl.mkProduct_[A, B](x.l))(y => new EvalO[(A, B)] {
      override def l: InterLangTerm[(A, B)] = ltl.mkProduct__(x.l)(y.l)

      override def tmr: tm.ret = (x, y)

      override val tm = ptm[A, B]
    }))

  override def zro[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[((A, B)) => A] =
    aeval(ltl.zro[A, B])(_.get(ptm[A, B])._1)

  override def fst[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[((A, B)) => B] =
    aeval(ltl.fst[A, B])(_.get(ptm[A, B])._2)

  override def litD: Double => EvalO[Double] = d => new EvalO[Double] {
    override def l: InterLangTerm[Double] = ltl.litD(d)

    override def tmr: tm.ret = ()

    override val tm: EvalOMatch.Aux[Double, Unit] = new EvalOMatch[Double] {
      override type ret = Unit
    }
  }

  def stm[A, B]: EvalOMatch.Aux[Either[A, B], Either[EvalO[A], EvalO[B]]] = new EvalOMatch[Either[A, B]] {
    override type ret = Either[EvalO[A], EvalO[B]]
  }

  override def sumComm[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[Either[A, B] => Either[B, A]] =
    aeval(ltl.sumComm[A, B])(_.get(stm[A, B]) match {
      case Left(x) => right_(x)
      case Right(x) => left_(x)
    })

  override def sumAssocLR[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]):
  EvalO[Either[Either[A, B], C] => Either[A, Either[B, C]]] =
    aeval(ltl.sumAssocLR[A, B, C])(_.get(stm[Either[A, B], C]).left.map(_.get(stm[A, B])) match {
      case Left(Left(x)) => left_(x)
      case Left(Right(x)) => right_(left_(x))
      case Right(x) => right_(right_(x))
    })

  override def sumAssocRL[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]):
  EvalO[Either[A, Either[B, C]] => Either[Either[A, B], C]] =
    aeval(ltl.sumAssocRL[A, B, C])(_.get(stm[A, Either[B, C]]).right.map(_.get(stm[B, C])) match {
      case Left(x) => left_(left_(x))
      case Right(Left(x)) => left_(right_(x))
      case Right(Right(x)) => right_(x)
    })

  override def left[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[A => Either[A, B]] =
    aeval(ltl.left[A, B])(a => new EvalO[Either[A, B]] {
      override def l: InterLangTerm[Either[A, B]] = ltl.left_(a.l)

      override def tmr: tm.ret = Left(a)

      override val tm = stm[A, B]
    })

  override def right[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[B => Either[A, B]] =
    aeval(ltl.right[A, B])(b => new EvalO[Either[A, B]] {
      override def l: InterLangTerm[Either[A, B]] = ltl.right_(b.l)

      override def tmr: tm.ret = Right(b)

      override val tm = stm[A, B]
    })

  override def sumMatch[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]):
  EvalO[Either[A, B] => (A => C) => (B => C) => C] = aeval(ltl.sumMatch[A, B, C])(_.get(stm[A, B]) match {
    case Left(x) => C_(K_(Let_[A, C](x)))
    case Right(x) => K_(Let_[B, C](x))
  })

  override def Y[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[((A => B) => A => B) => A => B] =
    aeval(ltl.Y[A, B])(f => aeval(ltl.Y_(f.l))(a => app(app(f)(Y_(f)))(a)))

  override def Let[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[A => (A => B) => B] =
    aeval(ltl.Let[A, B])(a => aeval(ltl.Let_[A, B](a.l))(f => app(f)(a)))

  override def W[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[(A => A => B) => A => B] =
    aeval(ltl.W[A, B])(f => aeval(ltl.W_[A, B](f.l))(a => app(app(f)(a))(a)))

  override def K[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]): EvalO[A => B => A] =
    aeval(ltl.K[A, B])(a => aeval(ltl.K_[A, B](a.l))(_ => a))

  override def curry[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]):
  EvalO[(((A, B)) => C) => A => B => C] = aeval(ltl.curry[A, B, C])(f =>
    aeval(ltl.curry_(f.l))(a => aeval(ltl.curry__(f.l)(a.l))(b => app(f)(mkProduct__(a)(b)))))

  override def S[A, B, C](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B], ci: InterLangInfoG[C]):
  EvalO[(A => B => C) => (A => B) => A => C] =
    aeval(ltl.S[A, B, C])(f => aeval(ltl.S_(f.l))(x => aeval(ltl.S__(f.l)(x.l))(a => app(app(f)(a))(app(x)(a)))))

  override def expD: EvalO[Double => Double] = aeval(ltl.expD)(x => litD(Math.exp(eval(x))))

  override def litB: Boolean => EvalO[Boolean] = b => new EvalO[Boolean] {
    override def l: InterLangTerm[Boolean] = ltl.litB(b)

    override def tmr: tm.ret = ()

    override val tm: EvalOMatch.Aux[Boolean, Unit] = new EvalOMatch[Boolean] {
      override type ret = Unit
    }
  }

  override def ite[A](implicit ai: InterLangInfoG[A]): EvalO[Boolean => A => A => A] =
    aeval(ltl.ite[A])(b => eval(b) match {
      case true => K[A, A]
      case false => C_(K[A, A])
    })

  override def reprInfo[A]: EvalO[A] => InterLangInfoG[A] = x => ltl.reprInfo(x.l)

  override implicit def botInfo: InterLangInfoG[Nothing] = new InterLangInfoG[Nothing] {
    override def apply[Info[_], Repr[_]](implicit lang: InterLang[Info, Repr]): Info[Nothing] = lang.botInfo
  }

  override def exfalso[A](implicit ai: InterLangInfoG[A]): EvalO[Nothing => A] = aeval[Nothing, A](ltl.exfalso[A])(eval)
}

object EvalOInterLang extends EvalOInterLang