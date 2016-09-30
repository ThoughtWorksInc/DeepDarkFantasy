package com.thoughtworks.DDF

import com.thoughtworks.DDF.Combinators.Comb
import com.thoughtworks.DDF.Lang.Lang
import com.thoughtworks.DDF.Unit.UnitLang

//Exists only for pedantic purpose. Use NextLang instead.
case class NaiveNextLang[Info[_], Repr[_], Arg](base: Lang[Info, Repr])(implicit argt: Info[Arg]) extends
  Comb[Lambda[X => Info[Arg => X]], Lambda[X => Repr[Arg => X]]] with
  UnitLang[Lambda[X => Info[Arg => X]], Lambda[X => Repr[Arg => X]]]{

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
    base.S[Arg, A, B](argt, base.ArrRngInfo(ReprInfo(x)), base.ArrRngInfo(base.ArrRngInfo(ReprInfo(f)))))(f))(x)

  override def ReprInfo[A]: Repr[Arg => A] => Info[Arg => A] = base.ReprInfo

  def rconv[X]: Repr[X] => Repr[Arg => X] = r => base.app(base.K[X, Arg](base.ReprInfo(r), argt))(r)

  def iconv[X]: Info[X] => Info[Arg => X] = x => base.ArrInfo[Arg, X](argt, x)

  def in: Repr[Arg => Arg] = base.I

  override def Y[A, B](implicit at: Info[Arg => A], bt: Info[Arg => B]):
  Repr[(Arg) => ((A => B) => A => B) => A => B] = rconv(base.Y[A, B](base.ArrRngInfo(at), base.ArrRngInfo(bt)))

  override def B[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]):
  Repr[Arg => (B => C) => (A => B) => A => C] =
    rconv(base.B[A, B, C](base.ArrRngInfo(ai), base.ArrRngInfo(bi), base.ArrRngInfo(ci)))

  override def W[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]): Repr[Arg => (A => A => B) => A => B] =
    rconv(base.W[A, B](base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def C[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]):
  Repr[Arg => (A => B => C) => B => A => C] =
    rconv(base.C[A, B, C](base.ArrRngInfo(ai), base.ArrRngInfo(bi), base.ArrRngInfo(ci)))

  override def ArrInfo[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]): Info[Arg => A => B] =
    iconv(base.ArrInfo[A, B](base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def mkUnit: Repr[Arg => Unit] = rconv(base.mkUnit)

  override implicit def UnitInfo: Info[Arg => Unit] = iconv(base.UnitInfo)
}