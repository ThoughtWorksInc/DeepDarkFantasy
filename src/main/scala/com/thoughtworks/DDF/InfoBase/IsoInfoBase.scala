package com.thoughtworks.DDF.InfoBase

import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import scalaz.Isomorphism._

trait IsoInfoBase[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  InfoBase[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] {
  override def reprInfo[A]: NRepr[A] => NInfo[A] = r => iconv(l.reprInfo(convr(r)))
}

object IsoInfoBase {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoInfoBase[OInfo, NInfo, ORepr, NRepr] =
    new IsoInfoBase[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}