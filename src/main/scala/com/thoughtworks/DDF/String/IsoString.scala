package com.thoughtworks.DDF.String

import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.{IsoLang, Lang}
import scalaz.Isomorphism._

trait IsoString[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  String[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] {
  override def stringInfo: NInfo[scala.Predef.String] = iconv[scala.Predef.String](l.stringInfo)

  override def litString = x => rconv[scala.Predef.String](l.litString(x))

  override def stringApp = rconv(l.stringApp)
}

object IsoString {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoLang[OInfo, NInfo, ORepr, NRepr] =
    new IsoLang[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}