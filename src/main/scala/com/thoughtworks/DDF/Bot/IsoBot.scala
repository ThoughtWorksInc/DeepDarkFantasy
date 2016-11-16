package com.thoughtworks.DDF.Bot

import com.thoughtworks.DDF.Arrow.IsoArr
import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import com.thoughtworks.DDF.Top.IsoTop
import scalaz.Isomorphism._

trait IsoBot[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  Bot[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] with
  IsoTop[OInfo, NInfo, ORepr, NRepr] with
  IsoArr[OInfo, NInfo, ORepr, NRepr] {
  override def exfalso[A](implicit ai: NInfo[A]) = rconv(l.exfalso[A])

  override implicit def botInfo: NInfo[Nothing] = iconv[Nothing](l.botInfo)

  override def imfalso[A](implicit ai: NInfo[A]) = rconv(l.imfalso[A])

  override def impossible = rconv(l.impossible)
}

object IsoBot {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoBot[OInfo, NInfo, ORepr, NRepr] =
    new IsoBot[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}