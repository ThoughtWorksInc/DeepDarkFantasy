package com.thoughtworks.DDF.RI

trait NaiveNextRI[Info[_], Repr[_], Arg] extends
  RILang[Lambda[X => Info[Arg => X]], Lambda[X => Repr[Arg => X]]] {
  implicit def base : RILang[Info, Repr]

  override def ReprInfo[A]: Repr[Arg => A] => Info[Arg => A] = base.ReprInfo
}
