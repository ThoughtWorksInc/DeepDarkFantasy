package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.NextArrow
import com.thoughtworks.DDF.Combinators.SKIRepr

trait NextBool[Info[_], Repr[_], Arg] extends
  BoolRepr[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with
  NextArrow[Info, Repr, Arg] {
  override implicit def base: BoolRepr[Info, Repr]

  override def litB = b => rconv(base.litB(b))

  override implicit def ite[A](implicit ai: Info[Arg => A]) = rconv(base.ite(base.arrowRangeInfo(ai)))

  override def BoolInfo: Info[Arg => Boolean] = iconv(base.BoolInfo)
}

object NextBool {
  implicit def apply[Info[_], Repr[_], Arg](implicit
                                            bool: BoolRepr[Info, Repr],
                                            skir: SKIRepr[Info, Repr],
                                            arg: Info[Arg]) = new NextBool[Info, Repr, Arg] {
    override implicit def base: BoolRepr[Info, Repr] = bool

    override implicit def argi: Info[Arg] = arg

    override implicit def ski: SKIRepr[Info, Repr] = skir
  }
}