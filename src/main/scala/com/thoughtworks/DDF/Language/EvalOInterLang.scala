package com.thoughtworks.DDF.Language

import com.thoughtworks.DDF.Combinators.EvalOComb
import com.thoughtworks.DDF.List.EvalOList
import com.thoughtworks.DDF.{EvalO, EvalOMatch}

trait EvalOInterLang extends
  InterLang[InterLangInfoG, EvalO] with
  ILIGLangInfo[EvalO] with
  EvalOComb with
  EvalOList {
  override def ltD: EvalO[Double => Double => Boolean] =
    aeval(ltl.ltD)(l => aeval(ltl.ltD_(l.l))(r => litB(eval(l) < eval(r))))

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

  override def optionMatch[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]):
  EvalO[Option[A] => B => (A => B) => B] =
    aeval(ltl.optionMatch[A, B])(_.get(otm[A]) match {
      case None => K[B, A => B]
      case Some(x) => K_[(A => B) => B, B](Let_[A, B](x))
    })

  override def plusD: EvalO[Double => Double => Double] =
    aeval(ltl.plusD)(l => aeval(ltl.plusD_(l.l))(r => litD(eval(l) + eval(r))))

  override def mkTop: EvalO[Unit] = new EvalO[Unit] {
    override def l: InterLangTerm[Unit] = ltl.mkTop

    override def tmr: tm.ret = ()

    override val tm: EvalOMatch.Aux[Unit, Unit] = new EvalOMatch[Unit] {
      override type ret = Unit
    }
  }

  override def sigD: EvalO[Double => Double] = aeval(ltl.sigD)(x => litD(1 / (1 + Math.exp(-eval(x)))))

  override def multD: EvalO[Double => Double => Double] =
    aeval(ltl.multD)(l => aeval(ltl.multD_(l.l))(r => litD(eval(l) * eval(r))))

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

  override def sumComm[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]):
  EvalO[Either[A, B] => Either[B, A]] =
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

  override def exfalso[A](implicit ai: InterLangInfoG[A]): EvalO[Nothing => A] = aeval[Nothing, A](ltl.exfalso[A])(eval)

  def IOeval[A]: InterLangTerm[IO[A]] => EvalO[IO[A]] = lio => new EvalO[IO[A]] {
    override def l: InterLangTerm[IO[A]] = lio

    override val tm = new EvalOMatch[IO[A]] {
      override type ret = Unit
    }

    override def tmr: tm.ret = ()
  }

  override def putDouble: EvalO[Double => IO[Unit]] =
    aeval(ltl.putDouble)(d => IOeval(ltl.putDouble_(ltl.litD(eval(d)))))

  override def IOBind[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]):
  EvalO[IO[A] => (A => IO[B]) => IO[B]] = aeval(ltl.IOBind[A, B])(ioa => aeval(ltl.IOBind_[A, B](ioa.l))(f =>
    IOeval[B](ltl.IOBind__(ioa.l)(f.l))))

  override def IORet[A](implicit ai: InterLangInfoG[A]):
  EvalO[A => IO[A]] = aeval(ltl.IORet[A])(a => IOeval(ltl.IORet_(a.l)))

  override def getDouble: EvalO[IO[Double]] = IOeval(ltl.getDouble)

  override def IOInfo[A](implicit ai: InterLangInfoG[A]): InterLangInfoG[EvalOInterLang.IO[A]] = ltl.IOInfo[A]

  override def IOElmInfo[A]: InterLangInfoG[EvalOInterLang.IO[A]] => InterLangInfoG[A] = ltl.IOElmInfo[A]

  def streamTM[A] = new EvalOMatch[Stream[A]] {
    override type ret = Option[(EvalO[A], EvalO[Unit => Stream[A]])]
  }

  override def streamNil[A](implicit ai: InterLangInfoG[A]): EvalO[Stream[A]] = new EvalO[Stream[A]] {
    override def l: InterLangTerm[Stream[A]] = ltl.streamNil[A]

    override def tmr: tm.ret = None

    override val tm = streamTM[A]
  }

  override def streamCons[A](implicit ai: InterLangInfoG[A]): EvalO[A => (Unit => Stream[A]) => Stream[A]] =
    aeval(ltl.streamCons[A])(a => aeval(ltl.streamCons_(a.l))(ls => new EvalO[Stream[A]] {
      override val tm = streamTM[A]

      override def l: InterLangTerm[Stream[A]] = ltl.streamCons__(a.l)(ls.l)

      override def tmr = Some((a, ls))
    }))

  override def streamMatch[A, B](implicit ai: InterLangInfoG[A], bi: InterLangInfoG[B]):
  EvalO[Stream[A] => B => (A => Stream[A] => B) => B] = aeval(ltl.streamMatch[A, B])(_.get(streamTM[A]) match {
    case None => K[B, (A => Stream[A] => B)]
    case Some((h, t)) => K_(B__(Let_[Stream[A], B](app(t)(mkTop)))(Let_[A, Stream[A] => B](h)))
  })

  override def recipD: EvalO[Double => Double] = aeval(ltl.recipD)(x => litD(1 / eval(x)))

  override def stringInfo: InterLangInfoG[String] = ltl.stringInfo

  override def litString: String => EvalO[String] = str => new EvalO[String] {
    override def l: InterLangTerm[String] = ltl.litString(str)

    override def tmr: tm.ret = ()

    override val tm: EvalOMatch.Aux[String, Unit] = new EvalOMatch[String] {
      override type ret = Unit
    }
  }
}

object EvalOInterLang extends EvalOInterLang