package com.thoughtworks.DDF

import com.thoughtworks.DDF.Combinators.SKIRepr

trait NaiveNextBase[Info[_], Repr[_], Arg] {
  implicit def argi: Info[Arg]

  implicit def ski: SKIRepr[Info, Repr]

  def rconv[X]: Repr[X] => Repr[Arg => X] = r => ski.app(ski.K[X, Arg](ski.ReprInfo(r), argi))(r)

  def iconv[X]: Info[X] => Info[Arg => X] = x => ski.ArrowInfo[Arg, X](argi, x)

  def in: Repr[Arg => Arg] = ski.I
}
