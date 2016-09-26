package com.thoughtworks.DDF

trait PairLang[Info[_], Repr[_]] extends ArrLang[Info, Repr] {
  def PairInfo[A, B]: Info[A] => Info[B] => Info[(A, B)]

  def PairFstInfo[A, B]: Info[(A, B)] => Info[A]

  def PairSndInfo[A, B]: Info[(A, B)] => Info[B]

  def mkPair[A, B](implicit at: Info[A], bt: Info[B]): Repr[A => B => (A, B)]

  def fst[A, B](implicit at: Info[A], bt: Info[B]): Repr[((A, B)) => A]

  def snd[A, B](implicit at: Info[A], bt: Info[B]): Repr[((A, B)) => B]
}
