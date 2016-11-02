package com.thoughtworks.DDF.Language

import scalaz.Isomorphism._

trait IsoLang[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends Lang[NInfo, NRepr] {
  def infoIso: OInfo <~> NInfo

  def reprIso: ORepr <~> NRepr

  def l: Lang[OInfo, ORepr]

  override def scanRight[A, B](implicit ai: NInfo[A], bi: NInfo[B]) =
    reprIso.to(l.scanRight[A, B](infoIso.from(ai), infoIso.from(bi)))

  override def zro[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[((A, B)) => A] = ???

  override def right[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[(B) => Either[A, B]] = ???

  override def ltD: NRepr[(Double) => (Double) => Boolean] = ???

  override def contRet[R, A](implicit ri: NInfo[R], ai: NInfo[A]): NRepr[(A) => Cont[R, A]] = ???

  override def scanLeft[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[((B) => (A) => B) => (B) => (List[A]) => List[B]] = ???

  override def listZip[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[(List[A]) => (List[B]) => List[(A, B)]] = ???

  override def divD: NRepr[(Double) => (Double) => Double] = ???

  override def foldRight[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[((A) => (B) => B) => (B) => (List[A]) => B] = ???

  override def none[A](implicit ai: NInfo[A]): NRepr[Option[A]] = ???

  override def some[A](implicit ai: NInfo[A]): NRepr[(A) => Option[A]] = ???

  override def optionMatch[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[(Option[A]) => (B) => ((A) => B) => B] = ???

  override def exceptBind[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]): NRepr[(Except[A, B]) => ((B) => Except[A, C]) => Except[A, C]] = ???

  override def reverse[A](implicit ai: NInfo[A]): NRepr[(List[A]) => List[A]] = ???

  override def cons[A](implicit ai: NInfo[A]): NRepr[(A) => (List[A]) => List[A]] = ???

  override def app[A, B]: (NRepr[(A) => B]) => (NRepr[A]) => NRepr[B] = ???

  override implicit def optionInfo[A](implicit ai: NInfo[A]): NInfo[Option[A]] = ???

  override def optionElmInfo[A]: (NInfo[Option[A]]) => NInfo[A] = ???

  override def listMap[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[((A) => B) => (List[A]) => List[B]] = ???

  override def expD: NRepr[(Double) => Double] = ???

  override def sumComm[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[(Either[A, B]) => Either[B, A]] = ???

  override def C[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]): NRepr[((A) => (B) => C) => (B) => (A) => C] = ???

  override def App[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[((A) => B) => (A) => B] = ???

  override def uncurry[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]): NRepr[((A) => (B) => C) => ((A, B)) => C] = ???

  override def I[A](implicit ai: NInfo[A]): NRepr[(A) => A] = ???

  override implicit def aInfo[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NInfo[(A) => B] = ???

  override def domInfo[A, B]: (NInfo[(A) => B]) => NInfo[A] = ???

  override def rngInfo[A, B]: (NInfo[(A) => B]) => NInfo[B] = ???

  override implicit def topInfo: NInfo[Unit] = ???

  override def foldLeft[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[((A) => (B) => A) => (A) => (List[B]) => A] = ???

  override def mkUnit: NRepr[Unit] = ???

  override implicit def doubleInfo: NInfo[Double] = ???

  override def sumAssocRL[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]): NRepr[(Either[A, Either[B, C]]) => Either[Either[A, B], C]] = ???

  override def fst[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[((A, B)) => B] = ???

  override def litB: (Boolean) => NRepr[Boolean] = ???

  override implicit def sumInfo[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NInfo[Either[A, B]] = ???

  override def sumLeftInfo[A, B]: (NInfo[Either[A, B]]) => NInfo[A] = ???

  override def sumRightInfo[A, B]: (NInfo[Either[A, B]]) => NInfo[B] = ???

  override def Y[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[(((A) => B) => (A) => B) => (A) => B] = ???

  override def Let[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[(A) => ((A) => B) => B] = ???

  override def nil[A](implicit ai: NInfo[A]): NRepr[List[A]] = ???

  override def listMatch[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[(List[A]) => (B) => ((A) => (List[A]) => B) => B] = ???

  override def W[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[((A) => (A) => B) => (A) => B] = ???

  override def sumMatch[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]): NRepr[(Either[A, B]) => ((A) => C) => ((B) => C) => C] = ???

  override def ite[A](implicit ai: NInfo[A]): NRepr[(Boolean) => (A) => (A) => A] = ???

  override def sigD: NRepr[(Double) => Double] = ???

  override def plusD: NRepr[(Double) => (Double) => Double] = ???

  override def multD: NRepr[(Double) => (Double) => Double] = ???

  override def K[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[(A) => (B) => A] = ???

  override def curry[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]): NRepr[(((A, B)) => C) => (A) => (B) => C] = ???

  override def left[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[(A) => Either[A, B]] = ???

  override def S[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]): NRepr[((A) => (B) => C) => ((A) => B) => (A) => C] = ???

  override implicit def prodInfo[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NInfo[(A, B)] = ???

  override def prodZroInfo[A, B]: (NInfo[(A, B)]) => NInfo[A] = ???

  override def prodFstInfo[A, B]: (NInfo[(A, B)]) => NInfo[B] = ???

  override def sumAssocLR[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]): NRepr[(Either[Either[A, B], C]) => Either[A, Either[B, C]]] = ???

  override def B[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]): NRepr[((B) => C) => ((A) => B) => (A) => C] = ???

  override implicit def boolInfo: NInfo[Boolean] = ???

  override implicit def listInfo[A](implicit ai: NInfo[A]): NInfo[List[A]] = ???

  override def listElmInfo[A]: (NInfo[List[A]]) => NInfo[A] = ???

  override def litD: (Double) => NRepr[Double] = ???

  override def contBind[R, A, B](implicit ri: NInfo[R], ai: NInfo[A], bi: NInfo[B]):
  NRepr[(Cont[R, A]) => ((A) => Cont[R, B]) => Cont[R, B]] = ???

  override def mkProduct[A, B](implicit ai: NInfo[A], bi: NInfo[B]): NRepr[(A) => (B) => (A, B)] = ???

  override def reprInfo[A]: NRepr[A] => NInfo[A] = ???

  override def exfalso[A](implicit ai: NInfo[A]): NRepr[Nothing => A] = ???

  override implicit def botInfo: NInfo[Nothing] = ???

  override def imfalso[A](implicit ai: NInfo[A]): NRepr[Unit => A] = ???

  override def impossible: NRepr[Unit => Nothing] = ???
}

object IsoLang {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]]: IsoLang[OInfo, NInfo, ORepr, NRepr] =
    new IsoLang[OInfo, NInfo, ORepr, NRepr] {
      override def infoIso: OInfo <~> NInfo = ???

      override def reprIso: ORepr <~> NRepr = ???

      override def l: Lang[OInfo, ORepr] = ???
    }
}
