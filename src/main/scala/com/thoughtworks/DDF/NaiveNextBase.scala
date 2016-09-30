package com.thoughtworks.DDF

import com.thoughtworks.DDF.Combinators.SKILang

trait NaiveNextBase[Info[_], Repr[_], Arg] {
  implicit def argi: Info[Arg]

  implicit def ski: SKILang[Info, Repr]

  def rconv[X]: Repr[X] => Repr[Arg => X] = r => ski.app(ski.K[X, Arg](ski.ReprInfo(r), argi))(r)

  def iconv[X]: Info[X] => Info[Arg => X] = x => ski.ArrInfo[Arg, X](argi, x)

  def in: Repr[Arg => Arg] = ski.I
}
