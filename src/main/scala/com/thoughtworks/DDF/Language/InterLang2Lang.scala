package com.thoughtworks.DDF.Language

trait InterLang2Lang[Info[_], Repr[_]] extends Lang[Info, Repr] {
  def i: InterLang[Info, Repr]

  override def scanRight[A, B](implicit ai: Info[A], bi: Info[B]) = i.scanRight[A, B]

  override def divD = i.divD

  override def scanLeft[A, B](implicit ai: Info[A], bi: Info[B]) = i.scanLeft[A, B]

  override def zeroth[A, B](implicit ai: Info[A], bi: Info[B]) = i.zeroth[A, B]

  override def right[A, B](implicit ai: Info[A], bi: Info[B]) = i.right[A, B]

  override def ltD = i.ltD

  override def sumComm[A, B](implicit ai: Info[A], bi: Info[B]) = i.sumComm[A, B]

  override def C[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]) = i.C[A, B, C]

  override def App[A, B](implicit ai: Info[A], bi: Info[B]) = i.App[A, B]

  override def reverse[A](implicit ai: Info[A]) = i.reverse[A]

  override def foldLeft[A, B](implicit ai: Info[A], bi: Info[B]) = i.foldLeft[A, B]

  override def uncurry[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]) = i.uncurry[A, B, C]

  override def plusD = i.plusD

  override def sigD = i.sigD

  override def app[A, B] = i.app[A, B]

  override implicit def optionInfo[A](implicit ai: Info[A]) = i.optionInfo[A]

  override def optionElmInfo[A] = i.optionElmInfo[A]

  override def listMap[A, B](implicit ai: Info[A], bi: Info[B]) = i.listMap[A, B]

  override def listZip[A, B](implicit ai: Info[A], bi: Info[B]) = i.listZip[A, B]

  override def none[A](implicit ai: Info[A]) = i.none[A]

  override def some[A](implicit ai: Info[A]) = i.some[A]

  override def optionMatch[A, B](implicit ai: Info[A], bi: Info[B]) = i.optionMatch[A, B]

  override def cons[A](implicit ai: Info[A]) = i.cons[A]

  override implicit def aInfo[A, B](implicit ai: Info[A], bi: Info[B]) = i.aInfo[A, B]

  override def domInfo[A, B] = i.domInfo[A, B]

  override def rngInfo[A, B] = i.rngInfo[A, B]

  override implicit def boolInfo = i.boolInfo

  override implicit def unitInfo = i.unitInfo

  override def foldRight[A, B](implicit ai: Info[A], bi: Info[B]) = i.foldRight[A, B]

  override def litD = i.litD

  override implicit def doubleInfo = i.doubleInfo

  override def sumAssocRL[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]) = i.sumAssocRL[A, B, C]

  override def first[A, B](implicit ai: Info[A], bi: Info[B]) = i.first[A, B]

  override def multD = i.multD

  override def sumMatch[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]) = i.sumMatch[A, B, C]

  override implicit def sumInfo[A, B](implicit ai: Info[A], bi: Info[B]) = i.sumInfo[A, B]

  override def sumLeftInfo[A, B] = i.sumLeftInfo[A, B]

  override def sumRightInfo[A, B] = i.sumRightInfo[A, B]

  override def Y[A, B](implicit ai: Info[A], bi: Info[B]) = i.Y[A, B]

  override def I[A](implicit ai: Info[A]) = i.I[A]

  override def Let[A, B](implicit ai: Info[A], bi: Info[B]) = i.Let[A, B]

  override def nil[A](implicit ai: Info[A]) = i.nil[A]

  override def listMatch[A, B](implicit ai: Info[A], bi: Info[B]) = i.listMatch[A, B]

  override def W[A, B](implicit ai: Info[A], bi: Info[B]) = i.W[A, B]

  override def ite[A](implicit ai: Info[A]) = i.ite[A]

  override def K[A, B](implicit ai: Info[A], bi: Info[B]) = i.K[A, B]

  override def curry[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]) = i.curry[A, B, C]

  override def left[A, B](implicit ai: Info[A], bi: Info[B]) = i.left[A, B]

  override def S[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]) = i.S[A, B, C]

  override def contBind[R, A, B](implicit ri: Info[R], ai: Info[A], bi: Info[B]):
  Repr[Cont[R, A] => (A => Cont[R, B]) => Cont[R, B]] = {
    val nLang = NextInterLang(i, aInfo(aInfo(ai, ri), ri))
    val nNLang = NextInterLang.apply[Info, nLang.repr, A => Cont[R, B]](nLang, aInfo(ai, aInfo(aInfo(bi, ri), ri)))
    val nnnl = NextInterLang.apply[Info, nNLang.repr, B => R](nNLang, aInfo(bi, ri))
    nLang.collapse(nNLang.collapse(nnnl.collapse(
      nnnl.app(
        nnnl.rconv(nNLang.rconv(nLang.in)))(
        nnnl.app(nnnl.C_(nnnl.rconv(nNLang.in)))(nnnl.in)))))
  }

  override implicit def productInfo[A, B](implicit ai: Info[A], bi: Info[B]) = i.productInfo[A, B]

  override def productZerothInfo[A, B] = i.productZerothInfo[A, B]

  override def productFirstInfo[A, B] = i.productFirstInfo[A, B]

  override def sumAssocLR[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]) = i.sumAssocLR[A, B, C]

  override def contRet[R, A](implicit ri: Info[R], ai: Info[A]): Repr[A => Cont[R, A]] = i.Let[A, R]

  override def expD = i.expD

  override def litB = i.litB

  override def B[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]) = i.B[A, B, C]

  override implicit def listInfo[A](implicit ai: Info[A]) = i.listInfo[A]

  override def listElmInfo[A] = i.listElmInfo[A]

  override def mkProduct[A, B](implicit ai: Info[A], bi: Info[B]) = i.mkProduct[A, B]

  override def mkUnit = i.mkUnit

  override def reprInfo[A] = i.reprInfo[A]
}

object InterLang2Lang {
  implicit def apply[Info[_], Repr[_]](implicit il: InterLang[Info, Repr]): InterLang2Lang[Info, Repr] =
    new InterLang2Lang[Info, Repr] {
      override def i: InterLang[Info, Repr] = il
    }
}
