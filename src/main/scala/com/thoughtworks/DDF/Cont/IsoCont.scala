package com.thoughtworks.DDF.Cont

import com.thoughtworks.DDF.Arrow.IsoArr
import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import scalaz.Isomorphism._

trait IsoCont[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  Cont[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] with
  IsoArr[OInfo, NInfo, ORepr, NRepr] {
  override def contRet[R, A](implicit ri: NInfo[R], ai: NInfo[A]) = rconv(l.contRet[R, A])

  override def contBind[R, A, B](implicit ri: NInfo[R], ai: NInfo[A], bi: NInfo[B]) = rconv(l.contBind[R, A, B])
}

object IsoCont {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoCont[OInfo, NInfo, ORepr, NRepr] =
    new IsoCont[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}