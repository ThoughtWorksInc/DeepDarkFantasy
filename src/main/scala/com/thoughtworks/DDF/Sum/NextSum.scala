package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.{ArrowRepr, NextArrow}
import com.thoughtworks.DDF.Combinators.SKIRepr

trait NextSum[Info[_], Repr[_], Arg] extends
  SumRepr[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with
  NextArrow[Info, Repr, Arg] {
  override def left[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.left(convi(ai), convi(bi)))

  override def right[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.right(convi(ai), convi(bi)))

  override def sumMatch[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]) =
    rconv(base.sumMatch(convi(ai), convi(bi), convi(ci)))

  override implicit def sumInfo[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) = 
    iconv(base.sumInfo(convi(ai), convi(bi)))

  override def sumLeftInfo[A, B] = x => iconv(base.sumLeftInfo(convi(x)))

  override def sumRightInfo[A, B] = x => iconv(base.sumRightInfo(convi(x)))

  implicit def base: SumRepr[Info, Repr]

  override def sumComm[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.sumComm(convi(ai), convi(bi)))

  override def sumAssocLR[A, B, C](implicit ai: Info[(Arg) => A], bi: Info[(Arg) => B], ci: Info[(Arg) => C]) =
    rconv(base.sumAssocLR(convi(ai), convi(bi), convi(ci)))

  override def sumAssocRL[A, B, C](implicit ai: Info[(Arg) => A], bi: Info[(Arg) => B], ci: Info[(Arg) => C]) =
    rconv(base.sumAssocRL(convi(ai), convi(bi), convi(ci)))
}

object NextSum {
  implicit def apply[Info[_], Repr[_], Arg](implicit
                                            sum: SumRepr[Info, Repr],
                                            skir: SKIRepr[Info, Repr],
                                            arg: Info[Arg]) = new NextSum[Info, Repr, Arg] {
    override def base: SumRepr[Info, Repr] = sum

    override implicit def argi: Info[Arg] = arg

    override implicit def ski: SKIRepr[Info, Repr] = skir
  }
}