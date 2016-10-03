package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.Combinators.SKIRepr
import com.thoughtworks.DDF.NextBase

trait NextArrow[Info[_], Repr[_], Arg] extends
  ArrowRepr[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with
  NextBase[Info, Repr, Arg] {
  def base: ArrowRepr[Info, Repr]

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

  override def reprInfo[A]: Either[Repr[A], Repr[Arg => A]] => Info[Arg => A] = {
    case Left(x) => iconv(base.reprInfo(x))
    case Right(x) => base.reprInfo(x)
  }
}

object NextArrow {
  implicit def apply[Info[_], Repr[_], Arg](implicit skir: SKIRepr[Info, Repr], arg: Info[Arg]) =
    new NextArrow[Info, Repr, Arg] {
      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKIRepr[Info, Repr] = skir

      override def base: ArrowRepr[Info, Repr] = ski
  }
}