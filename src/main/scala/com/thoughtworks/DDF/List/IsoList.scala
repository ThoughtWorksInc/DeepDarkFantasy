package com.thoughtworks.DDF.List

import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import com.thoughtworks.DDF.Product.IsoProd
import scalaz.Isomorphism._

trait IsoList[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  List[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] with
  IsoProd[OInfo, NInfo, ORepr, NRepr] {
  override def scanLeft[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.scanLeft[A, B])

  override def scanRight[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.scanRight[A, B])

  override def listZip[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.listZip[A, B])

  override def foldLeft[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.foldLeft[A, B])

  override def foldRight[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.foldRight[A, B])

  override def listMatch[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.listMatch[A, B])

  override def nil[A](implicit ai: NInfo[A]) = rconv(l.nil[A])

  override def cons[A](implicit ai: NInfo[A]) = rconv(l.cons[A])

  override def listMap[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.listMap[A, B])

  override implicit def listInfo[A](implicit ai: NInfo[A]) = iconv(l.listInfo[A])

  override def listElmInfo[A] = li => iconv(l.listElmInfo(convi(li)))

  override def reverse[A](implicit ai: NInfo[A]) = rconv(l.reverse[A])
}

object IsoList {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoList[OInfo, NInfo, ORepr, NRepr] =
    new IsoList[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}