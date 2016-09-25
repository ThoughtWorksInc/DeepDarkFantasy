package com.thoughtworks.DDF

import scala.language.higherKinds

object Next {

  case class NextLanguage[Info[_], Repr[_], Arg](base: Language[Info, Repr])(implicit argt: Info[Arg]) extends
    Language[Lambda[X => Info[Arg => X]], Lambda[X => Repr[Arg => X]]] {
    override def ArrInfo[A, B]: Info[Arg => A] => Info[Arg => B] => Info[Arg => A => B] = l => r =>
      iconv(base.ArrInfo(base.ArrRngInfo(l))(base.ArrRngInfo(r)))

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

    override def LitD: Double => Repr[Arg => Double] = x => rconv(base.LitD(x))

    override def PlusD: Repr[Arg => Double => Double => Double] = rconv(base.PlusD)

    override def MultD: Repr[Arg => Double => Double => Double] = rconv(base.MultD)

    override def app[A, B] = f => x => base.app(base.app(
      base.S[Arg, A, B](argt, base.ArrRngInfo(ReprInfo(x)), base.ArrRngInfo(base.ArrRngInfo(ReprInfo(f)))))(f))(x)

    override def ReprInfo[A]: Repr[Arg => A] => Info[Arg => A] = base.ReprInfo

    def rconv[X]: Repr[X] => Repr[Arg => X] = r => base.app(base.K[X, Arg](base.ReprInfo(r), argt))(r)

    def iconv[X]: Info[X] => Info[Arg => X] = base.ArrInfo[Arg, X](argt)

    def in: Repr[Arg => Arg] = base.I

    override def Y[A, B](implicit at: Info[Arg => A], bt: Info[Arg => B]):
    Repr[(Arg) => ((A => B) => A => B) => A => B] = rconv(base.Y[A, B](base.ArrRngInfo(at), base.ArrRngInfo(bt)))

    override def mkPair[A, B](implicit at: Info[Arg => A], bt: Info[Arg => B]): Repr[Arg => A => B => (A, B)] =
      rconv(base.mkPair[A, B](base.ArrRngInfo(at), base.ArrRngInfo(bt)))

    override def fst[A, B](implicit at: Info[(Arg) => A], bt: Info[(Arg) => B]): Repr[(Arg) => ((A, B)) => A] =
      rconv(base.fst(base.ArrRngInfo(at), base.ArrRngInfo(bt)))

    override def snd[A, B](implicit at: Info[(Arg) => A], bt: Info[(Arg) => B]): Repr[(Arg) => ((A, B)) => B] =
      rconv(base.snd(base.ArrRngInfo(at), base.ArrRngInfo(bt)))

    override def PairInfo[A, B]: (Info[(Arg) => A]) => (Info[(Arg) => B]) => Info[(Arg) => (A, B)] = a => b =>
      iconv(base.PairInfo(base.ArrRngInfo(a))(base.ArrRngInfo(b)))

    override def PairFstInfo[A, B]: Info[Arg => (A, B)] => Info[Arg => A] = p =>
      iconv(base.PairFstInfo(base.ArrRngInfo(p)))

    override def PairSndInfo[A, B]: Info[Arg => (A, B)] => Info[Arg => B] = p =>
      iconv(base.PairSndInfo(base.ArrRngInfo(p)))

    override def DoubleInfo: Info[Arg => Double] = iconv(base.DoubleInfo)

    override def left[A, B](implicit at: Info[Arg => A], bt: Info[Arg => B]): Repr[Arg => A => Either[A, B]] =
      rconv(base.left[A, B](base.ArrRngInfo(at), base.ArrRngInfo(bt)))

    override def right[A, B](implicit at: Info[Arg => A], bt: Info[Arg => B]): Repr[Arg => B => Either[A, B]] =
      rconv(base.right[A, B](base.ArrRngInfo(at), base.ArrRngInfo(bt)))

    override def sumMatch[A, B, C](implicit at: Info[Arg => A], bt: Info[Arg => B], ct: Info[Arg => C]):
    Repr[Arg => (A => C) => (B => C) => (Either[A, B]) => C] =
      rconv(base.sumMatch[A, B, C](base.ArrRngInfo(at), base.ArrRngInfo(bt), base.ArrRngInfo(ct)))

    override def SumInfo[A, B]: Info[Arg => A] => Info[Arg => B] => Info[Arg => Either[A, B]] = a => b =>
      iconv(base.SumInfo(base.ArrRngInfo(a))(base.ArrRngInfo(b)))

    override def SumLeftInfo[A, B]: Info[Arg => Either[A, B]] => Info[Arg => A] = ab =>
      iconv(base.SumLeftInfo(base.ArrRngInfo(ab)))

    override def SumRightInfo[A, B]: Info[Arg => Either[A, B]] => Info[Arg => B] = ab =>
      iconv(base.SumRightInfo(base.ArrRngInfo(ab)))
  }

}
