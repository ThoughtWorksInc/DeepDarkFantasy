package com.thoughtworks.DDF.Stream

import com.thoughtworks.DDF.Arrow.IsoArr
import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import com.thoughtworks.DDF.Top.IsoTop

import scalaz.Isomorphism._

trait IsoStream[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  Stream[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] with
  IsoArr[OInfo, NInfo, ORepr, NRepr] with
  IsoTop[OInfo, NInfo, ORepr, NRepr] {
  override def streamNil[A](implicit ai: NInfo[A]) = rconv(l.streamNil[A])

  override def streamCons[A](implicit ai: NInfo[A]) = rconv(l.streamCons[A])

  override def streamMatch[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.streamMatch[A, B])

  override implicit def streamInfo[A](implicit ai: NInfo[A]) = iconv(l.streamInfo[A](convi))
}

object IsoStream {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoStream[OInfo, NInfo, ORepr, NRepr] =
    new IsoStream[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}