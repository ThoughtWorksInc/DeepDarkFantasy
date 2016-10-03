package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.NextArrow
import com.thoughtworks.DDF.Combinators.{Comb, NextComb, SKIRepr}
import com.thoughtworks.DDF.NextBase

trait NextDouble[Info[_], Repr[_], Arg] extends
  DoubleRepr[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with
  NextBase[Info, Repr, Arg] with
  NextArrow[Info, Repr, Arg] {
  override def LitD = d => rconv(base.LitD(d))

  override def PlusD = rconv(base.PlusD)

  override def MultD = rconv(base.MultD)

  override implicit def DoubleInfo: Info[Arg => Double] = iconv(base.DoubleInfo)

  override implicit def base: DoubleRepr[Info, Repr]
}

object NextDouble {
  implicit def apply[Info[_], Repr[_], Arg](implicit
                                            double: DoubleRepr[Info, Repr],
                                            skir: SKIRepr[Info, Repr],
                                            arg: Info[Arg]) =
    new NextDouble[Info, Repr, Arg] {
      override implicit def base: DoubleRepr[Info, Repr] = double

      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKIRepr[Info, Repr] = skir
  }
}