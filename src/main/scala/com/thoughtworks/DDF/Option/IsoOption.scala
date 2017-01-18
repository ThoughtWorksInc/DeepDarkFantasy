package com.thoughtworks.DDF.Option

import com.thoughtworks.DDF.Arrow.IsoArr
import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import scalaz.Isomorphism._

trait IsoOption[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  Option[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] with
  IsoArr[OInfo, NInfo, ORepr, NRepr] {
  override def none[A](implicit ai: NInfo[A]) = rconv(l.none[A])

  override def some[A](implicit ai: NInfo[A]) = rconv(l.some[A])

  override def optionMatch[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.optionMatch[A, B])

  override implicit def optionInfo[A](implicit ai: NInfo[A]) = iconv(l.optionInfo[A](convi[A]))
}

object IsoOption {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoOption[OInfo, NInfo, ORepr, NRepr] =
    new IsoOption[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}