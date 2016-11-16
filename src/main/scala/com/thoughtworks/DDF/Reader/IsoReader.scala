package com.thoughtworks.DDF.Reader

import com.thoughtworks.DDF.Arrow.IsoArr
import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import scalaz.Isomorphism._

trait IsoReader[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  Reader[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] with
  IsoArr[OInfo, NInfo, ORepr, NRepr] {
  override def readerRet[E, A](implicit ei: NInfo[E], ai: NInfo[A]) = rconv(l.readerRet[E, A])

  override def readerBind[E, A, B](implicit ei: NInfo[E], ai: NInfo[A], bi: NInfo[B]) = rconv(l.readerBind[E, A, B])
}

object IsoReader {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoReader[OInfo, NInfo, ORepr, NRepr] =
    new IsoReader[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}