package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.NaiveNextBase

trait NaiveNextSKI[Info[_], Repr[_], Arg] extends
  SKILang[Lambda[X => Info[Arg => X]], Lambda[X => Repr[Arg => X]]] with
  NaiveNextBase[Info, Repr, Arg] {
  implicit def base: SKILang[Info, Repr]

  override def ArrDomInfo[A, B]: Info[Arg => A => B] => Info[Arg => A] = x =>
    iconv(base.ArrDomInfo(base.ArrRngInfo(x)))

  override def ArrRngInfo[A, B]: Info[Arg => A => B] => Info[Arg => B] = x =>
    iconv(base.ArrRngInfo(base.ArrRngInfo(x)))

  override def S[A, B, C](implicit at: Info[Arg => A], bt: Info[Arg => B], ct: Info[Arg => C]):
  Repr[Arg => (A => B => C) => (A => B) => A => C] =
    rconv(base.S[A, B, C](base.ArrRngInfo(at), base.ArrRngInfo(bt), base.ArrRngInfo(ct)))

  override def K[A, B](implicit at: Info[Arg => A], bt: Info[Arg => B]): Repr[Arg => A => B => A] =
    rconv(base.K[A, B](base.ArrRngInfo(at), base.ArrRngInfo(bt)))

  override def I[A](implicit at: Info[Arg => A]): Repr[Arg => A => A] = rconv(base.I[A](base.ArrRngInfo(at)))

  override def app[A, B] = f => x => base.app(base.app(
    base.S[Arg, A, B](argi, base.ArrRngInfo(ReprInfo(x)), base.ArrRngInfo(base.ArrRngInfo(ReprInfo(f)))))(f))(x)

  override def ReprInfo[A]: Repr[Arg => A] => Info[Arg => A] = base.ReprInfo

  override def ArrInfo[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]): Info[Arg => A => B] =
    iconv(base.ArrInfo[A, B](base.ArrRngInfo(ai), base.ArrRngInfo(bi)))
}

object NaiveNextSKI {
  implicit def apply[Info[_], Repr[_], Arg](implicit skil: SKILang[Info, Repr], arg: Info[Arg]) =
    new NaiveNextSKI[Info, Repr, Arg] {
      override implicit def base: SKILang[Info, Repr] = skil

      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKILang[Info, Repr] = skil
    }
}