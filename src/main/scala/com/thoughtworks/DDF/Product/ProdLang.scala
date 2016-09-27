package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arr.ArrLang

trait ProdLang[Info[_], Repr[_]] extends ArrLang[Info, Repr] {
  def ProdInfo[A, B]: Info[A] => Info[B] => Info[(A, B)]

  def ProdFstInfo[A, B]: Info[(A, B)] => Info[A]

  def ProdSndInfo[A, B]: Info[(A, B)] => Info[B]

  def mkProd[A, B](implicit at: Info[A], bt: Info[B]): Repr[A => B => (A, B)]

  def fst[A, B](implicit at: Info[A], bt: Info[B]): Repr[((A, B)) => A]

  def snd[A, B](implicit at: Info[A], bt: Info[B]): Repr[((A, B)) => B]

  def curry[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(((A, B)) => C) => A => B => C]

  def uncurry[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]): Repr[(A => B => C) => ((A, B)) => C]
}
