package com.thoughtworks.DDF.Except

import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import com.thoughtworks.DDF.Sum.IsoSum
import scalaz.Isomorphism._

trait IsoExcept[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  Except[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] with
  IsoSum[OInfo, NInfo, ORepr, NRepr] {
  override def exceptBind[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.exceptBind[A, B, C])
}

object IsoExcept {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoExcept[OInfo, NInfo, ORepr, NRepr] =
    new IsoExcept[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}