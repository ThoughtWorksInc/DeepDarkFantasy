package com.thoughtworks.DDF.LangBase

import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import scalaz.Isomorphism._

trait IsoLangBase[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  LangBase[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] {
  override def reprInfo[A]: NRepr[A] => NInfo[A] = r => iconv(l.reprInfo(convr(r)))
}

object IsoLangBase {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoLangBase[OInfo, NInfo, ORepr, NRepr] =
    new IsoLangBase[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}