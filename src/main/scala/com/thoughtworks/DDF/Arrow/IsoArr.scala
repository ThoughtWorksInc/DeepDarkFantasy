package com.thoughtworks.DDF.Arrow

import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import scalaz.Isomorphism._

trait IsoArr[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  Arr[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] {
  override implicit def aInfo[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = iconv(l.aInfo[A, B])

  override def app[A: NInfo, B: NInfo](f: NRepr[A => B])(x: NRepr[A]): NRepr[B] = rconv(l.app(convr(f))(convr(x)))
}

object IsoArr {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoArr[OInfo, NInfo, ORepr, NRepr] =
    new IsoArr[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}