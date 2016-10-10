package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.Arrow.{ArrowRepr, NextArrow}
import com.thoughtworks.DDF.Combinators.SKIRepr

trait NextList [Info[_], Repr[_], Arg] extends
  ListRepr[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with
  NextArrow[Info, Repr, Arg] {
  override implicit def base: ListRepr[Info, Repr]

  override implicit def listInfo[A](implicit ai: Info[Arg => A]) = iconv(base.listInfo(convi(ai)))

  override def listElmInfo[A](implicit lai: Info[Arg => List[A]]) = iconv(base.listElmInfo(convi(lai)))

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
}

object NextList {
  implicit def apply[Info[_], Repr[_], Arg](implicit
                                            list: ListRepr[Info, Repr],
                                            skir: SKIRepr[Info, Repr],
                                            arg: Info[Arg])  =
    new NextList[Info, Repr, Arg] {
      override implicit def base: ListRepr[Info, Repr] = list

      override implicit def ski: SKIRepr[Info, Repr] = skir

      override implicit def argi: Info[Arg] = arg
    }
}