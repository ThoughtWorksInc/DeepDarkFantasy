package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.Combinators.SKIRepr
import com.thoughtworks.DDF.NextBase

trait NextArrow[Info[_], Repr[_], Arg] extends
  ArrowRepr[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with
  NextBase[Info, Repr, Arg] {
  def base: ArrowRepr[Info, Repr]

  override def ArrowInfo[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    iconv(base.ArrowInfo[A, B](base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def ArrDomInfo[A, B] = x => iconv(base.ArrDomInfo(base.ArrRngInfo(x)))

  override def ArrRngInfo[A, B] = x => iconv(base.ArrRngInfo(base.ArrRngInfo(x)))

  override def app[A, B] = f => x => (f, x) match {
    case (Left(l), Left(r)) => Left(base.app(l)(r))
    case (Right(l), Right(r)) =>
      Right(base.app(base.app(ski.S[Arg, A, B](
        argi,
        base.ArrDomInfo(base.ArrRngInfo(base.ReprInfo(l))),
        base.ArrRngInfo(base.ArrRngInfo(base.ReprInfo(l)))))(l))(r))
    case (Left(l), Right(r)) => app[A, B](Right(lift(l)))(Right(r))
    case (Right(l), Left(r)) => app[A, B](Right(l))(Right(lift(r)))
  }

  override def ReprInfo[A]: Either[Repr[A], Repr[Arg => A]] => Info[Arg => A] = {
    case Left(x) => iconv(base.ReprInfo(x))
    case Right(x) => base.ReprInfo(x)
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