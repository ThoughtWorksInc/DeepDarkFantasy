package com.thoughtworks.DDF.Bool

import com.thoughtworks.DDF.Arrow.IsoArr
import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import scalaz.Isomorphism._

trait IsoBool[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  Bool[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] with
  IsoArr[OInfo, NInfo, ORepr, NRepr] {
  override def litB = x => rconv(l.litB(x))

  override def ite[A](implicit ai: NInfo[A]) = rconv(l.ite[A])

  override implicit def boolInfo = iconv(l.boolInfo)
}

object IsoBool {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoBool[OInfo, NInfo, ORepr, NRepr] =
    new IsoBool[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}