package com.thoughtworks.DDF.InfoB

trait NaiveNextInfoB[Info[_], Repr[_], Arg] extends
  InfoB[Lambda[X => Info[Arg => X]], Lambda[X => Repr[Arg => X]]] {
  implicit def base : InfoB[Info, Repr]

  override def reprInfo[A]: Repr[Arg => A] => Info[Arg => A] = base.reprInfo
}
