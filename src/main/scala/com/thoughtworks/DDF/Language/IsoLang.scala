package com.thoughtworks.DDF.Language

import scalaz.Isomorphism._

trait IsoLang[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends Lang[NInfo, NRepr] {
  def infoIso: OInfo <~> NInfo

  def reprIso: ORepr <~> NRepr

  def l: Lang[OInfo, ORepr]

  def rconv[A]: ORepr[A] => NRepr[A] = reprIso.to.apply[A]

  def convr[A]: NRepr[A] => ORepr[A] = reprIso.from.apply[A]

  def iconv[A]: OInfo[A] => NInfo[A] = infoIso.to.apply[A]

  implicit def convi[A](implicit n: NInfo[A]): OInfo[A] = infoIso.from(n)

  override def scanRight[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.scanRight[A, B])

  override def zro[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.zro[A, B])

  override def right[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.right[A, B])

  override def ltD = rconv(l.ltD)

  override def contRet[R, A](implicit ri: NInfo[R], ai: NInfo[A]) = rconv(l.contRet[R, A])

  override def scanLeft[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.scanLeft[A, B])

  override def listZip[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.listZip[A, B])

  override def divD = rconv(l.divD)

  override def foldRight[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.foldRight[A, B])

  override def none[A](implicit ai: NInfo[A]) = rconv(l.none[A])

  override def some[A](implicit ai: NInfo[A]) = rconv(l.some[A])

  override def optionMatch[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.optionMatch[A, B])

  override def exceptBind[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.exceptBind[A, B, C])

  override def reverse[A](implicit ai: NInfo[A]) = rconv(l.reverse[A])

  override def cons[A](implicit ai: NInfo[A]) = rconv(l.cons[A])

  override def app[A, B]: NRepr[A => B] => NRepr[A] => NRepr[B] = f => x => rconv(l.app(convr(f))(convr(x)))

  override implicit def optionInfo[A](implicit ai: NInfo[A]) = iconv(l.optionInfo[A])

  override def optionElmInfo[A] = oi => iconv(l.optionElmInfo[A](convi(oi)))

  override def listMap[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.listMap[A, B])

  override def expD = rconv(l.expD)

  override def sumComm[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.sumComm[A, B])

  override def C[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.C[A, B, C])

  override def App[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.App[A, B])

  override def uncurry[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.uncurry[A, B, C])

  override def I[A](implicit ai: NInfo[A]) = rconv(l.I[A])

  override implicit def aInfo[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = iconv(l.aInfo[A, B])

  override def domInfo[A, B] = ai => iconv(l.domInfo(convi(ai)))

  override def rngInfo[A, B] = ai => iconv(l.rngInfo(convi(ai)))

  override implicit def topInfo = iconv(l.topInfo)

  override def foldLeft[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.foldLeft[A, B])

  override def mkTop: NRepr[Unit] = rconv(l.mkTop)

  override implicit def doubleInfo = iconv(l.doubleInfo)

  override def sumAssocRL[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.sumAssocRL[A, B, C])

  override def fst[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.fst[A, B])

  override def litB = x => rconv(l.litB(x))

  override implicit def sumInfo[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = iconv(l.sumInfo[A, B])

  override def sumLeftInfo[A, B] = si => iconv(l.sumLeftInfo(convi(si)))

  override def sumRightInfo[A, B] = si => iconv(l.sumRightInfo(convi(si)))

  override def Y[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.Y[A, B])

  override def Let[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.Let[A, B])

  override def nil[A](implicit ai: NInfo[A]) = rconv(l.nil[A])

  override def listMatch[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.listMatch[A, B])

  override def W[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.W[A, B])

  override def sumMatch[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.sumMatch[A, B, C])

  override def ite[A](implicit ai: NInfo[A]) = rconv(l.ite[A])

  override def sigD = rconv(l.sigD)

  override def plusD = rconv(l.plusD)

  override def multD = rconv(l.multD)

  override def K[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.K[A, B])

  override def curry[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.curry[A, B, C])

  override def left[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.left[A, B])

  override def S[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.S[A, B, C])

  override implicit def prodInfo[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = iconv(l.prodInfo[A, B])

  override def prodZroInfo[A, B] = pi => iconv(l.prodZroInfo(convi(pi)))

  override def prodFstInfo[A, B] = pi => iconv(l.prodFstInfo(convi(pi)))

  override def sumAssocLR[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.sumAssocLR[A, B, C])

  override def B[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.B[A, B, C])

  override implicit def boolInfo = iconv(l.boolInfo)

  override implicit def listInfo[A](implicit ai: NInfo[A]) = iconv(l.listInfo[A])

  override def listElmInfo[A] = li => iconv(l.listElmInfo(convi(li)))

  override def litD = d => rconv(l.litD(d))

  override def contBind[R, A, B](implicit ri: NInfo[R], ai: NInfo[A], bi: NInfo[B]) = rconv(l.contBind[R, A, B])

  override def mkProd[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.mkProd[A, B])

  override def reprInfo[A]: NRepr[A] => NInfo[A] = r => iconv(l.reprInfo(convr(r)))

  override def exfalso[A](implicit ai: NInfo[A]) = rconv(l.exfalso[A])

  override implicit def botInfo: NInfo[Nothing] = iconv[Nothing](l.botInfo)

  override def imfalso[A](implicit ai: NInfo[A]) = rconv(l.imfalso[A])

  override def impossible = rconv(l.impossible)

  override def readerRet[E, A](implicit ei: NInfo[E], ai: NInfo[A]) = rconv(l.readerRet[E, A])

  override def readerBind[E, A, B](implicit ei: NInfo[E], ai: NInfo[A], bi: NInfo[B]) = rconv(l.readerBind[E, A, B])

  override def stateRet[S, A](implicit si: NInfo[S], ai: NInfo[A]) = rconv(l.stateRet[S, A])

  override def stateBind[S, A, B](implicit si: NInfo[S], ai: NInfo[A], bi: NInfo[B]) = rconv(l.stateBind[S, A, B])

  override def putDouble = rconv(l.putDouble)

  override def IOBind[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.IOBind[A, B])

  override def IORet[A](implicit ai: NInfo[A]) = rconv(l.IORet[A])

  override def getDouble: NRepr[IO[Double]] = rconv(l.getDouble)

  override def IOInfo[A](implicit ai: NInfo[A]): NInfo[IO[A]] = iconv(l.IOInfo[A])

  override def IOElmInfo[A]: NInfo[IO[A]] => NInfo[A] = ioa => iconv(l.IOElmInfo(convi(ioa)))

  override def streamNil[A](implicit ai: NInfo[A]): NRepr[Stream[A]] = rconv(l.streamNil[A])

  override def streamCons[A](implicit ai: NInfo[A]) = rconv(l.streamCons[A])

  override def streamMatch[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.streamMatch[A, B])

  override implicit def streamInfo[A](implicit ai: NInfo[A]): NInfo[Stream[A]] = iconv(l.streamInfo[A])

  override def streamElmInfo[A]: NInfo[Stream[A]] => NInfo[A] = si => iconv(l.streamElmInfo(convi(si)))
}

object IsoLang {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoLang[OInfo, NInfo, ORepr, NRepr] =
    new IsoLang[OInfo, NInfo, ORepr, NRepr] {
      override def infoIso: OInfo <~> NInfo = ii

      override def reprIso: ORepr <~> NRepr = ri

      override def l: Lang[OInfo, ORepr] = lang
    }
}
