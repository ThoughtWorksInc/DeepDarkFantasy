package com.thoughtworks.DDF.Language

import scalaz.{Forall, NaturalTransformation}

trait NTInterLang[Info[_], Repr[_], F[_]] extends InterLang[Info, F] {
  def base: InterLang[Info, Repr]

  def NTF: NaturalTransformation[Repr, F]

  override def K[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.K)

  override def ltD = NTF(base.ltD)

  override def App[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.App)

  override implicit def sumInfo[A, B](implicit ai: Info[A], bi: Info[B]) = base.sumInfo

  override def scanRight[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.scanRight)

  override def W[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.W)

  override def multD = NTF(base.multD)

  override def scanLeft[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.scanLeft)

  override def left[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.left)

  override def right[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.right)

  override def sumMatch[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]) = NTF(base.sumMatch)

  override def sigD = NTF(base.sigD)

  override def Let[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.Let)

  override implicit def aInfo[A, B](implicit ai: Info[A], bi: Info[B]) = base.aInfo

  override def mkTop = NTF(base.mkTop)

  override implicit def optionInfo[A](implicit ai: Info[A]) = base.optionInfo

  override def listZip[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.listZip)

  override implicit def listInfo[A](implicit ai: Info[A]) = base.listInfo

  override def listElmInfo[A] = base.listElmInfo

  override def C[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]) = NTF(base.C)

  override def foldLeft[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.foldLeft)

  override def plusD = NTF(base.plusD)

  override def none[A](implicit ai: Info[A]) = NTF(base.none)

  override def some[A](implicit ai: Info[A]) = NTF(base.some)

  override def optionMatch[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.optionMatch)

  override def reverse[A](implicit ai: Info[A]) = NTF(base.reverse)

  override def listMap[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.listMap)

  override def foldRight[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.foldRight)

  override implicit def prodInfo[A, B](implicit ai: Info[A], bi: Info[B]) = base.prodInfo

  override def litD = d => NTF(base.litD(d))

  override def litB = b => NTF(base.litB(b))

  override def ite[A](implicit ai: Info[A]) = NTF(base.ite)

  override def sumComm[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.sumComm)

  override def sumAssocLR[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]) = NTF(base.sumAssocLR)

  override def sumAssocRL[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]) = NTF(base.sumAssocRL)

  override implicit def topInfo = base.topInfo

  override def S[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]) = NTF(base.S)

  override def expD = NTF(base.expD)

  override implicit def boolInfo = base.boolInfo

  override def B[A, B, C](implicit ai: Info[A], bi: Info[B], ci: Info[C]) = NTF(base.B)

  override def I[A](implicit ai: Info[A]) = NTF(base.I)

  override def mkProd[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.mkProd)

  override def zro[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.zro)

  override def fst[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.fst)

  override def Z[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.Z)

  override def nil[A](implicit ai: Info[A]) = NTF(base.nil)

  override def cons[A](implicit ai: Info[A]) = NTF(base.cons)

  override def listMatch[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.listMatch)

  override implicit def doubleInfo = base.doubleInfo

  override def exfalso[A](implicit ai: Info[A]) = NTF(base.exfalso[A])

  override implicit def botInfo = base.botInfo

  override def putDouble = NTF(base.putDouble)

  override def IOBind[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.IOBind[A, B])

  override def getDouble = NTF(base.getDouble)

  override def IORet[A](implicit ai: Info[A]) = NTF(base.IORet[A])

  override def IOInfo[A](implicit ai: Info[A]) = base.IOInfo[A]

  override def IOElmInfo[A]: Info[IO[A]] => Info[A] = base.IOElmInfo[A]

  override def streamNil[A](implicit ai: Info[A]): F[Stream[A]] = NTF(base.streamNil[A])

  override def streamCons[A](implicit ai: Info[A]) = NTF(base.streamCons[A])

  override def streamMatch[A, B](implicit ai: Info[A], bi: Info[B]) = NTF(base.streamMatch[A, B])

  override implicit def streamInfo[A](implicit ai: Info[A]): Info[Stream[A]] = base.streamInfo[A]

  override def recipD = NTF(base.recipD)

  override def litString: (String) => F[String] = str => NTF(base.litString(str))

  override def stringInfo: Info[String] = base.stringInfo
}

object NTInterLang {
  implicit def apply[Info[_], Repr[_], F[_]](implicit
                                             l: InterLang[Info, Repr],
                                             n: NaturalTransformation[Repr, F],
                                             ap: Forall[Lambda[A => Forall[Lambda[B => F[A => B] => F[A] => F[B]]]]],
                                             r: NaturalTransformation[F, Info]): NTInterLang[Info, Repr, F] =
    new NTInterLang[Info, Repr, F] {
      override def base: InterLang[Info, Repr] = l

      override def NTF: NaturalTransformation[Repr, F] = n

      override def app[A, B] = ap.apply[A].apply[B]
    }
}
