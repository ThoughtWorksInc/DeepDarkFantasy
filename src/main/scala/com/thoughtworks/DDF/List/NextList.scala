package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.{ArrowRepr, NextArrow}
import com.thoughtworks.DDF.Combinators.SKI
import com.thoughtworks.DDF.Product.NextProduct

trait NextList [Info[_], Repr[_], Arg] extends
  List[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with
  NextArrow[Info, Repr, Arg] with
  NextProduct[Info, Repr, Arg] {
  override implicit def base: List[Info, Repr]

  override implicit def listInfo[A](implicit ai: Info[Arg => A]) = iconv(base.listInfo(convi(ai)))

  override def listElmInfo[A] = lai => iconv(base.listElmInfo(convi(lai)))

  override def nil[A](implicit ai: Info[Arg => A]) = rconv(base.nil(convi(ai)))

  override def cons[A](implicit ai: Info[Arg => A]) = rconv(base.cons(convi(ai)))

  override def listMatch[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.listMatch(convi(ai), convi(bi)))

  override def listMap[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.listMap(convi(ai), convi(bi)))

  override def reverse[A](implicit ai: Info[Arg => A]) = rconv(base.reverse(convi(ai)))

  override def foldRight[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.foldRight(convi(ai), convi(bi)))

  override def foldLeft[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.foldLeft(convi(ai), convi(bi)))

  override def listZip[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.listZip(convi(ai), convi(bi)))


  override def scanLeft[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.scanLeft(convi(ai), convi(bi)))

  override def scanRight[A, B](implicit ai: Info[(Arg) => A], bi: Info[(Arg) => B]) =
    rconv(base.scanRight(convi(ai), convi(bi)))
}

object NextList {
  implicit def apply[Info[_], Repr[_], Arg](implicit
                                            list: List[Info, Repr],
                                            skir: SKI[Info, Repr],
                                            arg: Info[Arg])  =
    new NextList[Info, Repr, Arg] {
      override implicit def base: List[Info, Repr] = list

      override implicit def ski: SKI[Info, Repr] = skir

      override implicit def argi: Info[Arg] = arg
    }
}