package com.thoughtworks.DDF.InfoBase

trait NaiveNextInfoBase[Info[_], Repr[_], Arg] extends
  InfoBase[Lambda[X => Info[Arg => X]], Lambda[X => Repr[Arg => X]]] {
  implicit def base : InfoBase[Info, Repr]

  override def reprInfo[A]: Repr[Arg => A] => Info[Arg => A] = base.reprInfo
}
