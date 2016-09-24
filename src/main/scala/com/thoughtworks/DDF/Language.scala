package com.thoughtworks.DDF

import scala.language.higherKinds

trait Language[Info[_], Repr[_]] {
  def ArrInfo[A, B]: Info[A] => Info[B] => Info[A => B]

  def ArrDomInfo[A, B]: Info[A => B] => Info[A]

  def ArrRngInfo[A, B]: Info[A => B] => Info[B]

  def PairInfo[A, B]: Info[A] => Info[B] => Info[(A, B)]

  def PairFstInfo[A, B]: Info[(A, B)] => Info[A]

  def PairSndInfo[A, B]: Info[(A, B)] => Info[B]

  def ReprInfo[A]: Repr[A] => Info[A]

  def DoubleInfo : Info[Double]

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
