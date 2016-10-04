package com.thoughtworks.DDF.Lang

import com.thoughtworks.DDF.Combinators.{NextComb, SKIRepr}
import com.thoughtworks.DDF.Double.NextDouble
import com.thoughtworks.DDF.NextBase
import com.thoughtworks.DDF.Option.NextOption
import com.thoughtworks.DDF.Product.NextProduct
import com.thoughtworks.DDF.Sum.NextSum

trait NextLang[Info[_], Repr[_], Arg] extends
  Lang[Lambda[X => Info[Arg => X]], Lambda[X => Either[Repr[X], Repr[Arg => X]]]] with
  NextBase[Info, Repr, Arg] with
  NextComb[Info, Repr, Arg] with
  NextDouble[Info, Repr, Arg] with
  NextProduct[Info, Repr, Arg] with
  NextOption[Info, Repr, Arg] with
  NextSum[Info, Repr, Arg] {
  implicit def base: Lang[Info, Repr]

  override implicit def listInfo[A](implicit ai: Info[Arg => A]) = iconv(base.listInfo(base.arrowRangeInfo(ai)))

  override def listElmInfo[A](implicit lai: Info[Arg => List[A]]) = iconv(base.listElmInfo(base.arrowRangeInfo(lai)))

  override implicit def unitInfo = iconv(base.unitInfo)

  override def mkUnit = rconv(base.mkUnit)

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
}

object NextLang {
  implicit def apply[Info[_], Repr[_], Arg](implicit lang: Lang[Info, Repr], arg: Info[Arg]) =
    new NextLang[Info, Repr, Arg] {
      override implicit def base: Lang[Info, Repr] = lang

      override implicit def argi: Info[Arg] = arg

      override implicit def ski: SKIRepr[Info, Repr] = lang
  }
}
