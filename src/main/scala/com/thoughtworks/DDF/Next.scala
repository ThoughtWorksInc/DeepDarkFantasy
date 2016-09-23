package com.thoughtworks.DDF

import scala.language.higherKinds

object Next {

  case class NextLanguage[Type[_], Repr[_], Arg](base: Language[Type, Repr])(implicit argt: Type[Arg]) extends
    Language[Lambda[X => Type[Arg => X]], Lambda[X => Repr[Arg => X]]] {
    override def Arr[A, B]: Type[Arg => A] => Type[Arg => B] => Type[Arg => A => B] = l => r =>
      base.Arr(base.ArrDom(l))(base.Arr(base.ArrRng(l))(base.ArrRng(r)))

    override def ArrDom[A, B]: Type[Arg => A => B] => Type[Arg => A] = x =>
      base.Arr(base.ArrDom(x))(base.ArrDom(base.ArrRng(x)))

    override def ArrRng[A, B]: Type[Arg => A => B] => Type[Arg => B] = x =>
      base.Arr(base.ArrDom(x))(base.ArrRng(base.ArrRng(x)))

    override def S[A, B, C](implicit at: Type[Arg => A], bt: Type[Arg => B], ct: Type[Arg => C]):
    Repr[Arg => (A => B => C) => (A => B) => A => C] = conv(base.S[A, B, C](base.ArrRng(at), base.ArrRng(bt), base.ArrRng(ct)))

    override def K[A, B](implicit at: Type[Arg => A], bt: Type[Arg => B]): Repr[Arg => A => B => A] =
      conv(base.K[A, B](base.ArrRng(at), base.ArrRng(bt)))

    override def I[A](implicit at: Type[Arg => A]): Repr[Arg => A => A] = conv(base.I[A](base.ArrRng(at)))

    override def LitD: Double => Repr[Arg => Double] = x => conv(base.LitD(x))

    override def PlusD: Repr[Arg => Double => Double => Double] = conv(base.PlusD)

    override def MultD: Repr[Arg => Double => Double => Double] = conv(base.MultD)

    override def app[A, B] = f => x => base.app(base.app(
      base.S[Arg, A, B](argt, base.ArrRng(ReprType(x)), base.ArrRng(base.ArrRng(ReprType(f)))))(f))(x)

    override def ReprType[A]: Repr[Arg => A] => Type[Arg => A] = r => base.ReprType(r)

    def conv[X]: Repr[X] => Repr[Arg => X] = r => base.app(base.K[X, Arg](base.ReprType(r), argt))(r)

    def in: Repr[Arg => Arg] = base.I

    override def Y[A, B](implicit at: Type[Arg => A], bt: Type[Arg => B]):
    Repr[(Arg) => ((A => B) => A => B) => A => B] = conv(base.Y[A, B](base.ArrRng(at), base.ArrRng(bt)))

    override def mkPair[A, B](implicit at: Type[Arg => A], bt: Type[Arg => B]): Repr[Arg => A => B => (A, B)] =
      conv(base.mkPair[A, B](base.ArrRng(at), base.ArrRng(bt)))

    override def fst[A, B](implicit at: Type[(Arg) => A], bt: Type[(Arg) => B]): Repr[(Arg) => ((A, B)) => A] =
      conv(base.fst(base.ArrRng(at), base.ArrRng(bt)))

    override def snd[A, B](implicit at: Type[(Arg) => A], bt: Type[(Arg) => B]): Repr[(Arg) => ((A, B)) => B] =
      conv(base.snd(base.ArrRng(at), base.ArrRng(bt)))

    override def Pair[A, B]: (Type[(Arg) => A]) => (Type[(Arg) => B]) => Type[(Arg) => (A, B)] = a => b =>
      base.Arr(base.ArrDom(a))(base.Pair(base.ArrRng(a))(base.ArrRng(b)))

    override def PairFst[A, B]: Type[Arg => (A, B)] => Type[Arg => A] = p => base.Arr(base.ArrDom(p))(base.PairFst(base.ArrRng(p)))

    override def PairSnd[A, B]: Type[Arg => (A, B)] => Type[Arg => B] = p => base.Arr(base.ArrDom(p))(base.PairSnd(base.ArrRng(p)))
  }

}
