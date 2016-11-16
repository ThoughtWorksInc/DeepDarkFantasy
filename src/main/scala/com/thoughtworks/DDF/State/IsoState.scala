package com.thoughtworks.DDF.State

import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import com.thoughtworks.DDF.Product.IsoProd
import scalaz.Isomorphism._

trait IsoState[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  State[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] with
  IsoProd[OInfo, NInfo, ORepr, NRepr] {
  override def stateRet[S, A](implicit si: NInfo[S], ai: NInfo[A]) = rconv(l.stateRet[S, A])

  override def stateBind[S, A, B](implicit si: NInfo[S], ai: NInfo[A], bi: NInfo[B]) = rconv(l.stateBind[S, A, B])
}

object IsoState {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoState[OInfo, NInfo, ORepr, NRepr] =
    new IsoState[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}