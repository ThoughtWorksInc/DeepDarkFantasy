package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Arrow.NextArrow
import com.thoughtworks.DDF.Combinators.{Comb, NextComb, SKI}
import com.thoughtworks.DDF.NextBase

trait NextDouble[Info[_], Repr[_], Arg] extends
  Double[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with
  NextBase[Info, Repr, Arg] with
  NextArrow[Info, Repr, Arg] {
  override def litD = d => rconv(base.litD(d))

  override def plusD = rconv(base.plusD)

  override def multD = rconv(base.multD)

  override def divD = rconv(base.divD)

  override def expD = rconv(base.expD)

  override def sigD = rconv(base.sigD)

  override implicit def doubleInfo = iconv(base.doubleInfo)

  override implicit def base: Double[Info, Repr]
}

object NextDouble {
  implicit def apply[Info[_], Repr[_], Arg](implicit
                                            double: Double[Info, Repr],
                                            skir: SKI[Info, Repr],
                                            arg: Info[Arg]) =
    new NextDouble[Info, Repr, Arg] {
      override implicit def base: Double[Info, Repr] = double

      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKI[Info, Repr] = skir
    }
}