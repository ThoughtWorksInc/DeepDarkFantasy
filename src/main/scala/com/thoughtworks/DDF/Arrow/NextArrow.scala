package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.Combinators.SKI
import com.thoughtworks.DDF.InfoBase.NextInfoBase

trait NextArrow[Info[_], Repr[_], Arg] extends
  Arrow[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with
  NextInfoBase[Info, Repr, Arg] {
  implicit def base: Arrow[Info, Repr]

  override def arrowInfo[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    iconv(base.arrowInfo[A, B](base.arrowRangeInfo(ai), base.arrowRangeInfo(bi)))

  override def arrowDomainInfo[A, B] = x => iconv(base.arrowDomainInfo(base.arrowRangeInfo(x)))

  override def arrowRangeInfo[A, B] = x => iconv(base.arrowRangeInfo(base.arrowRangeInfo(x)))

  override def app[A, B] = f => x => (f, x) match {
    case (Left(l), Left(r)) => Left(base.app(l)(r))
    case (Right(l), Right(r)) =>
      Right(base.app(base.app(ski.S[Arg, A, B](
        argi,
        base.arrowDomainInfo(base.arrowRangeInfo(base.reprInfo(l))),
        base.arrowRangeInfo(base.arrowRangeInfo(base.reprInfo(l)))))(l))(r))
    case (Left(l), Right(r)) => app[A, B](Right(lift(l)))(Right(r))
    case (Right(l), Left(r)) => app[A, B](Right(l))(Right(lift(r)))
  }
}

object NextArrow {
  implicit def apply[Info[_], Repr[_], Arg](implicit skir: SKI[Info, Repr], arg: Info[Arg]) =
    new NextArrow[Info, Repr, Arg] {
      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKI[Info, Repr] = skir

      override def base: Arrow[Info, Repr] = ski
  }
}