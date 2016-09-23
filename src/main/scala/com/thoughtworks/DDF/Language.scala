package com.thoughtworks.DDF

import scala.language.higherKinds

trait Language[TypeInfo[_], Repr[_]] {
  def Arr[A, B]: TypeInfo[A] => TypeInfo[B] => TypeInfo[A => B]

  def ArrDom[A, B]: TypeInfo[A => B] => TypeInfo[A]

  def ArrRng[A, B]: TypeInfo[A => B] => TypeInfo[B]

  def Pair[A, B]: TypeInfo[A] => TypeInfo[B] => TypeInfo[(A, B)]

  def PairFst[A, B]: TypeInfo[(A, B)] => TypeInfo[A]

  def PairSnd[A, B]: TypeInfo[(A, B)] => TypeInfo[B]

  def ReprType[A]: Repr[A] => TypeInfo[A]

  def app[A, B]: Repr[A => B] => Repr[A] => Repr[B]

  def S[A, B, C](implicit at: TypeInfo[A], bt: TypeInfo[B], ct: TypeInfo[C]): Repr[(A => B => C) => (A => B) => A => C]

  def K[A, B](implicit at: TypeInfo[A], bt: TypeInfo[B]): Repr[A => B => A]

  def I[A](implicit at: TypeInfo[A]): Repr[A => A]

  def LitD: Double => Repr[Double]

  def PlusD: Repr[Double => Double => Double]

  def MultD: Repr[Double => Double => Double]

  def Y[A, B](implicit at: TypeInfo[A], bt: TypeInfo[B]): Repr[((A => B) => (A => B)) => (A => B)]

  def mkPair[A, B](implicit at: TypeInfo[A], bt: TypeInfo[B]): Repr[A => B => (A, B)]

  def fst[A, B](implicit at: TypeInfo[A], bt: TypeInfo[B]): Repr[((A, B)) => A]

  def snd[A, B](implicit at: TypeInfo[A], bt: TypeInfo[B]): Repr[((A, B)) => B]
}
