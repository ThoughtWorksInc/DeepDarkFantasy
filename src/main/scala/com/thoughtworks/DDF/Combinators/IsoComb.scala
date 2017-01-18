package com.thoughtworks.DDF.Combinators

import com.thoughtworks.DDF.Arrow.IsoArr
import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import scalaz.Isomorphism._

trait IsoComb[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  Comb[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] with
  IsoArr[OInfo, NInfo, ORepr, NRepr] {
  override def B[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.B[A, B, C])

  override def C[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.C[A, B, C])

  override def K[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.K[A, B])

  override def W[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.W[A, B])

  override def App[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.App[A, B])

  override def Let[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.Let[A, B])

  override def S[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.S[A, B, C])

  override def I[A](implicit ai: NInfo[A]) = rconv(l.I[A])

  override def Z[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.Z[A, B])
}

object IsoComb {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoComb[OInfo, NInfo, ORepr, NRepr] =
    new IsoComb[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}