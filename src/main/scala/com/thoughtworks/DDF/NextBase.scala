package com.thoughtworks.DDF

import com.thoughtworks.DDF.Combinators.SKIRepr

trait NextBase[Info[_], Repr[_], Arg] {
  implicit def argi: Info[Arg]

  implicit def ski: SKIRepr[Info, Repr]

  def rconv[X]: Repr[X] => Either[Repr[X], Repr[Arg => X]] = x => Left(x)

  def iconv[X]: Info[X] => Info[Arg => X] = x => ski.ArrowInfo[Arg, X](argi, x)

  def in = Right(ski.I)
}
