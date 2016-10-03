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

  override implicit def ProdInfo[A, B](implicit ai: Info[(Arg) => A], bi: Info[(Arg) => B]) =
    iconv(base.ProdInfo(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def ProdFstInfo[A, B] = p => iconv(base.ProdFstInfo(base.ArrRngInfo(p)))

  override def ProdSndInfo[A, B] = p => iconv(base.ProdSndInfo(base.ArrRngInfo(p)))

  override implicit def SumInfo[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    iconv(base.SumInfo(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def SumLeftInfo[A, B] = ab => iconv(base.SumLeftInfo(base.ArrRngInfo(ab)))

  override def SumRightInfo[A, B] = ab => iconv(base.SumRightInfo(base.ArrRngInfo(ab)))

  override implicit def ListInfo[A](implicit ai: Info[Arg => A]) = iconv(base.ListInfo(base.ArrRngInfo(ai)))

  override def ListElmInfo[A](implicit lai: Info[Arg => List[A]]) = iconv(base.ListElmInfo(base.ArrRngInfo(lai)))

  override implicit def UnitInfo = iconv(base.UnitInfo)

  override def mkUnit = rconv(base.mkUnit)

  override def left[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.left(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def right[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.right(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def sumMatch[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]) =
    rconv(base.sumMatch(base.ArrRngInfo(ai), base.ArrRngInfo(bi), base.ArrRngInfo(ci)))

  override def mkProd[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.mkProd(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def fst[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.fst(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def snd[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.snd(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def curry[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]) =
    rconv(base.curry(base.ArrRngInfo(ai), base.ArrRngInfo(bi), base.ArrRngInfo(ci)))

  override def uncurry[A, B, C](implicit ai: Info[Arg => A], bi: Info[Arg => B], ci: Info[Arg => C]) =
    rconv(base.uncurry(base.ArrRngInfo(ai), base.ArrRngInfo(bi), base.ArrRngInfo(ci)))

  override def Nil[A](implicit ai: Info[Arg => A]) = rconv(base.Nil(base.ArrRngInfo(ai)))

  override def Cons[A](implicit ai: Info[Arg => A]) =
    rconv(base.Cons(base.ArrRngInfo(ai)))

  override def listMatch[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.listMatch(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def listMap[A, B](implicit ai: Info[Arg => A], bi: Info[Arg => B]) =
    rconv(base.listMap(base.ArrRngInfo(ai), base.ArrRngInfo(bi)))

  override def litB = b => rconv(base.litB(b))

  override implicit def ite[A](implicit ai: Info[Arg => A]) = rconv(base.ite(base.ArrRngInfo(ai)))

  override def BoolInfo: Info[Arg => Boolean] = iconv(base.BoolInfo)
}

object NextLang {
  implicit def apply[Info[_], Repr[_], Arg](implicit lang: Lang[Info, Repr], arg: Info[Arg]) = new NextLang[Info, Repr, Arg] {
    override implicit def base: Lang[Info, Repr] = lang

    override implicit def argi: Info[Arg] = arg

    override implicit def ski: SKIRepr[Info, Repr] = lang
  }
}
