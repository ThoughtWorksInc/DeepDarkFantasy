package com.thoughtworks.DDF.Sum

import com.thoughtworks.DDF.Arrow.IsoArr
import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import scalaz.Isomorphism._

trait IsoSum[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  Sum[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] with
  IsoArr[OInfo, NInfo, ORepr, NRepr] {
  override def sumAssocLR[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.sumAssocLR[A, B, C])

  override def sumAssocRL[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.sumAssocRL[A, B, C])

  override def left[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.left[A, B])

  override def right[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.right[A, B])

  override def sumComm[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.sumComm[A, B])

  override implicit def sumInfo[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = iconv(l.sumInfo[A, B])

  override def sumMatch[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.sumMatch[A, B, C])
}

object IsoSum {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoSum[OInfo, NInfo, ORepr, NRepr] =
    new IsoSum[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}