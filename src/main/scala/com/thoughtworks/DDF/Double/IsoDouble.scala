package com.thoughtworks.DDF.Double

import com.thoughtworks.DDF.Bool.IsoBool
import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import scalaz.Isomorphism._

trait IsoDouble[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  Double[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] with
  IsoBool[OInfo, NInfo, ORepr, NRepr] {
  override def ltD = rconv(l.ltD)

  override def expD = rconv(l.expD)

  override def recipD = rconv(l.recipD)

  override def negD = rconv(l.negD)

  override def divD = rconv(l.divD)

  override def minusD = rconv(l.minusD)

  override def sigD = rconv(l.sigD)

  override def plusD = rconv(l.plusD)

  override def multD = rconv(l.multD)

  override def litD = d => rconv(l.litD(d))

  override implicit def doubleInfo = iconv(l.doubleInfo)
}

object IsoDouble {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoDouble[OInfo, NInfo, ORepr, NRepr] =
    new IsoDouble[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}