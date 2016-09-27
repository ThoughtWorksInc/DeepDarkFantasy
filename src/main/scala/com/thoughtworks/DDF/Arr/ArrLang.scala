package com.thoughtworks.DDF.Arr

import com.thoughtworks.DDF.RI.RILang

trait ArrLang[Info[_], Repr[_]] extends RILang[Info, Repr] {
  implicit def ArrInfo[A, B](implicit ai : Info[A], bi : Info[B]) : Info[A => B]

  def ArrDomInfo[A, B]: Info[A => B] => Info[A]

  def ArrRngInfo[A, B]: Info[A => B] => Info[B]

  def app[A, B]: Repr[A => B] => Repr[A] => Repr[B]
}
