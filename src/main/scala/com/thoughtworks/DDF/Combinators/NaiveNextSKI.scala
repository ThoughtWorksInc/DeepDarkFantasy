package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.NaiveNextBase
import com.thoughtworks.DDF.InfoBase.NaiveNextInfoBase

trait NaiveNextSKI[Info[_], Repr[_], Arg] extends
  SKI[Lambda[X => Info[Arg => X]], Lambda[X => Repr[Arg => X]]] with
  NaiveNextBase[Info, Repr, Arg] with
  NaiveNextInfoBase[Info, Repr, Arg] {
  implicit def base: SKI[Info, Repr]

  override def arrowDomainInfo[A, B]: Info[Arg => A => B] => Info[Arg => A] = x =>
    iconv(base.arrowDomainInfo(base.arrowRangeInfo(x)))

  override def arrowRangeInfo[A, B]: Info[Arg => A => B] => Info[Arg => B] = x =>
    iconv(base.arrowRangeInfo(base.arrowRangeInfo(x)))

  override def S[A, B, C](implicit at: Info[Arg => A], bt: Info[Arg => B], ct: Info[Arg => C]):
  Repr[Arg => (A => B => C) => (A => B) => A => C] =
    rconv(base.S[A, B, C](base.arrowRangeInfo(at), base.arrowRangeInfo(bt), base.arrowRangeInfo(ct)))

  override def K[A, B](implicit at: Info[Arg => A], bt: Info[Arg => B]): Repr[Arg => A => B => A] =
    rconv(base.K[A, B](base.arrowRangeInfo(at), base.arrowRangeInfo(bt)))

  override def I[A](implicit at: Info[Arg => A]): Repr[Arg => A => A] = rconv(base.I[A](base.arrowRangeInfo(at)))

  override def app[A, B] = f => x => base.app(base.app(
    base.S[Arg, A, B](argi, base.arrowRangeInfo(reprInfo(x)), base.arrowRangeInfo(base.arrowRangeInfo(reprInfo(f)))))(f))(x)

  override def arrowInfo[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]): Info[Arg => A => B] =
    iconv(base.arrowInfo[A, B](base.arrowRangeInfo(ai), base.arrowRangeInfo(bi)))
}

object NaiveNextSKI {
  implicit def apply[Info[_], Repr[_], Arg](implicit skil: SKI[Info, Repr], arg: Info[Arg]) =
    new NaiveNextSKI[Info, Repr, Arg] {
      override implicit def base: SKI[Info, Repr] = skil

      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKI[Info, Repr] = skil
    }
}