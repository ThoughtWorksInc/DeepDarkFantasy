package com.thoughtworks.DDF

import scala.language.higherKinds

trait Language[Info[_], Repr[_]] {
  def Arr[A, B]: Info[A] => Info[B] => Info[A => B]

  def ArrDom[A, B]: Info[A => B] => Info[A]

  def ArrRng[A, B]: Info[A => B] => Info[B]

  def Pair[A, B]: Info[A] => Info[B] => Info[(A, B)]

  def PairFst[A, B]: Info[(A, B)] => Info[A]

  def PairSnd[A, B]: Info[(A, B)] => Info[B]

  def ReprType[A]: Repr[A] => Info[A]

  def app[A, B]: Repr[A => B] => Repr[A] => Repr[B]

  def S[A, B, C](implicit at: Info[A], bt: Info[B], ct: Info[C]): Repr[(A => B => C) => (A => B) => A => C]

  def K[A, B](implicit at: Info[A], bt: Info[B]): Repr[A => B => A]

  def I[A](implicit at: Info[A]): Repr[A => A]

  def LitD: Double => Repr[Double]

  def PlusD: Repr[Double => Double => Double]

  def MultD: Repr[Double => Double => Double]

  def Y[A, B](implicit at: Info[A], bt: Info[B]): Repr[((A => B) => (A => B)) => (A => B)]

  def mkPair[A, B](implicit at: Info[A], bt: Info[B]): Repr[A => B => (A, B)]

  def fst[A, B](implicit at: Info[A], bt: Info[B]): Repr[((A, B)) => A]

  def snd[A, B](implicit at: Info[A], bt: Info[B]): Repr[((A, B)) => B]
}
