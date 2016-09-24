package com.thoughtworks.DDF

import scala.language.higherKinds

object Next {

  case class NextLanguage[Info[_], Repr[_], Arg](base: Language[Info, Repr])(implicit argt: Info[Arg]) extends
    Language[Lambda[X => Info[Arg => X]], Lambda[X => Repr[Arg => X]]] {
    override def ArrInfo[A, B]: Info[Arg => A] => Info[Arg => B] => Info[Arg => A => B] = l => r =>
      base.ArrInfo(base.ArrDomInfo(l))(base.ArrInfo(base.ArrRngInfo(l))(base.ArrRngInfo(r)))

    override def ArrDomInfo[A, B]: Info[Arg => A => B] => Info[Arg => A] = x =>
      base.ArrInfo(base.ArrDomInfo(x))(base.ArrDomInfo(base.ArrRngInfo(x)))

    override def ArrRngInfo[A, B]: Info[Arg => A => B] => Info[Arg => B] = x =>
      base.ArrInfo(base.ArrDomInfo(x))(base.ArrRngInfo(base.ArrRngInfo(x)))

    override def S[A, B, C](implicit at: Info[Arg => A], bt: Info[Arg => B], ct: Info[Arg => C]):
    Repr[Arg => (A => B => C) => (A => B) => A => C] =
      conv(base.S[A, B, C](base.ArrRngInfo(at), base.ArrRngInfo(bt), base.ArrRngInfo(ct)))

    override def K[A, B](implicit at: Info[Arg => A], bt: Info[Arg => B]): Repr[Arg => A => B => A] =
      conv(base.K[A, B](base.ArrRngInfo(at), base.ArrRngInfo(bt)))

    override def I[A](implicit at: Info[Arg => A]): Repr[Arg => A => A] = conv(base.I[A](base.ArrRngInfo(at)))

    override def LitD: Double => Repr[Arg => Double] = x => conv(base.LitD(x))

    override def PlusD: Repr[Arg => Double => Double => Double] = conv(base.PlusD)

    override def MultD: Repr[Arg => Double => Double => Double] = conv(base.MultD)

    override def app[A, B] = f => x => base.app(base.app(
      base.S[Arg, A, B](argt, base.ArrRngInfo(ReprInfo(x)), base.ArrRngInfo(base.ArrRngInfo(ReprInfo(f)))))(f))(x)

    override def ReprInfo[A]: Repr[Arg => A] => Info[Arg => A] = r => base.ReprInfo(r)

    def conv[X]: Repr[X] => Repr[Arg => X] = r => base.app(base.K[X, Arg](base.ReprInfo(r), argt))(r)

    def in: Repr[Arg => Arg] = base.I

    override def Y[A, B](implicit at: Info[Arg => A], bt: Info[Arg => B]):
    Repr[(Arg) => ((A => B) => A => B) => A => B] = conv(base.Y[A, B](base.ArrRngInfo(at), base.ArrRngInfo(bt)))

    override def mkPair[A, B](implicit at: Info[Arg => A], bt: Info[Arg => B]): Repr[Arg => A => B => (A, B)] =
      conv(base.mkPair[A, B](base.ArrRngInfo(at), base.ArrRngInfo(bt)))

    override def fst[A, B](implicit at: Info[(Arg) => A], bt: Info[(Arg) => B]): Repr[(Arg) => ((A, B)) => A] =
      conv(base.fst(base.ArrRngInfo(at), base.ArrRngInfo(bt)))

    override def snd[A, B](implicit at: Info[(Arg) => A], bt: Info[(Arg) => B]): Repr[(Arg) => ((A, B)) => B] =
      conv(base.snd(base.ArrRngInfo(at), base.ArrRngInfo(bt)))

    override def PairInfo[A, B]: (Info[(Arg) => A]) => (Info[(Arg) => B]) => Info[(Arg) => (A, B)] = a => b =>
      base.ArrInfo(base.ArrDomInfo(a))(base.PairInfo(base.ArrRngInfo(a))(base.ArrRngInfo(b)))

    override def PairFstInfo[A, B]: Info[Arg => (A, B)] => Info[Arg => A] = p =>
      base.ArrInfo(base.ArrDomInfo(p))(base.PairFstInfo(base.ArrRngInfo(p)))

    override def PairSndInfo[A, B]: Info[Arg => (A, B)] => Info[Arg => B] = p =>
      base.ArrInfo(base.ArrDomInfo(p))(base.PairSndInfo(base.ArrRngInfo(p)))
  }

}
