package com.thoughtworks.DDF.Top

import com.thoughtworks.DDF.LangBase.IsoLangBase
import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import scalaz.Isomorphism._

trait IsoTop[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  Top[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] with
  IsoLangBase[OInfo, NInfo, ORepr, NRepr] {
  override implicit def topInfo = iconv(l.topInfo)

  override def mkTop: NRepr[Unit] = rconv(l.mkTop)
}

object IsoTop {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoTop[OInfo, NInfo, ORepr, NRepr] =
    new IsoTop[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}