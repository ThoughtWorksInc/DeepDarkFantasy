package com.thoughtworks.DDF.Product

import com.thoughtworks.DDF.Arrow.IsoArr
import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import scalaz.Isomorphism._

trait IsoProd[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  Prod[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] with
  IsoArr[OInfo, NInfo, ORepr, NRepr] {
  override def ><[A, B, C, D](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C], di: NInfo[D]) = rconv(l.><[A, B, C, D])

  override def zro[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.zro[A, B])

  override def fst[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.fst[A, B])

  override def uncurry[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.uncurry[A, B, C])

  override def curry[A, B, C](implicit ai: NInfo[A], bi: NInfo[B], ci: NInfo[C]) = rconv(l.curry[A, B, C])

  override implicit def prodInfo[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = iconv(l.prodInfo[A, B])

  override def prodZroInfo[A, B] = pi => iconv(l.prodZroInfo(convi(pi)))

  override def prodFstInfo[A, B] = pi => iconv(l.prodFstInfo(convi(pi)))

  override def mkProd[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.mkProd[A, B])
}

object IsoProd {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoProd[OInfo, NInfo, ORepr, NRepr] =
    new IsoProd[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}