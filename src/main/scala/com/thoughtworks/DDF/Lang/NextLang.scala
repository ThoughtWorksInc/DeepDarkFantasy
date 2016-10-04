package com.thoughtworks.DDF.Lang

import com.thoughtworks.DDF.Combinators.{NextComb, SKIRepr}
import com.thoughtworks.DDF.Double.NextDouble
import com.thoughtworks.DDF.NextBase

trait NextLang[Info[_], Repr[_], Arg] extends
  Lang[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with
  NextBase[Info, Repr, Arg] with
  NextComb[Info, Repr, Arg] with
  NextDouble[Info, Repr, Arg] {
  implicit def base: Lang[Info, Repr]

  override implicit def productInfo[A, B](implicit ai: Info[(Arg) => A], bi: Info[(Arg) => B]) =
    iconv(base.productInfo(base.arrowRangeInfo(ai), base.arrowRangeInfo(bi)))

  override def productZerothInfo[A, B] = p => iconv(base.productZerothInfo(base.arrowRangeInfo(p)))

  override def productFirstInfo[A, B] = p => iconv(base.productFirstInfo(base.arrowRangeInfo(p)))

  override implicit def sumInfo[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    iconv(base.sumInfo(base.arrowRangeInfo(ai), base.arrowRangeInfo(bi)))

  override def sumLeftInfo[A, B] = ab => iconv(base.sumLeftInfo(base.arrowRangeInfo(ab)))

  override def sumRightInfo[A, B] = ab => iconv(base.sumRightInfo(base.arrowRangeInfo(ab)))

  override implicit def listInfo[A](implicit ai: Info[Arg => A]) = iconv(base.listInfo(base.arrowRangeInfo(ai)))

  override def listElmInfo[A](implicit lai: Info[Arg => List[A]]) = iconv(base.listElmInfo(base.arrowRangeInfo(lai)))

  override implicit def unitInfo = iconv(base.unitInfo)

  override def mkUnit = rconv(base.mkUnit)

  override def left[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.left(base.arrowRangeInfo(ai), base.arrowRangeInfo(bi)))

  override def right[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.right(base.arrowRangeInfo(ai), base.arrowRangeInfo(bi)))

  override def sumMatch[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]) =
    rconv(base.sumMatch(base.arrowRangeInfo(ai), base.arrowRangeInfo(bi), base.arrowRangeInfo(ci)))

  override def mkProduct[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.mkProduct(base.arrowRangeInfo(ai), base.arrowRangeInfo(bi)))

  override def zeroth[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.zeroth(base.arrowRangeInfo(ai), base.arrowRangeInfo(bi)))

  override def first[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.first(base.arrowRangeInfo(ai), base.arrowRangeInfo(bi)))

  override def curry[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]) =
    rconv(base.curry(base.arrowRangeInfo(ai), base.arrowRangeInfo(bi), base.arrowRangeInfo(ci)))

  override def uncurry[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]) =
    rconv(base.uncurry(base.arrowRangeInfo(ai), base.arrowRangeInfo(bi), base.arrowRangeInfo(ci)))

  override def nil[A](implicit ai: Info[Arg => A]) = rconv(base.nil(base.arrowRangeInfo(ai)))

  override def cons[A](implicit ai: Info[Arg => A]) =
    rconv(base.cons(base.arrowRangeInfo(ai)))

  override def listMatch[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.listMatch(base.arrowRangeInfo(ai), base.arrowRangeInfo(bi)))

  override def listMap[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.listMap(base.arrowRangeInfo(ai), base.arrowRangeInfo(bi)))

  override def litB = b => rconv(base.litB(b))

  override implicit def ite[A](implicit ai: Info[Arg => A]) = rconv(base.ite(base.arrowRangeInfo(ai)))

  override def BoolInfo: Info[Arg => Boolean] = iconv(base.BoolInfo)

  override implicit def optionInfo[A](implicit ai: Info[Arg => A]) =
    iconv(base.optionInfo(base.arrowRangeInfo(ai)))

  override def optionElmInfo[A] = oai => iconv(base.optionElmInfo(base.arrowRangeInfo(oai)))

  override def none[A](implicit ai: Info[Arg => A]) = rconv(base.none(base.arrowRangeInfo(ai)))

  override def some[A](implicit ai: Info[Arg => A]) = rconv(base.some(base.arrowRangeInfo(ai)))

  override def optionMatch[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.optionMatch(base.arrowRangeInfo(ai), base.arrowRangeInfo(bi)))
}

object NextLang {
  implicit def apply[Info[_], Repr[_], Arg](implicit lang: Lang[Info, Repr], arg: Info[Arg]) =
    new NextLang[Info, Repr, Arg] {
      override implicit def base: Lang[Info, Repr] = lang

      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKIRepr[Info, Repr] = lang
  }
}
