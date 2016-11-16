package com.thoughtworks.DDF.IO

import com.thoughtworks.DDF.Double.IsoDouble
import com.thoughtworks.DDF.IsoBase
import com.thoughtworks.DDF.Language.Lang
import com.thoughtworks.DDF.Top.IsoTop
import scalaz.Isomorphism._

trait IsoIO[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] extends
  IO[NInfo, NRepr] with
  IsoBase[OInfo, NInfo, ORepr, NRepr] with
  IsoDouble[OInfo, NInfo, ORepr, NRepr] with
  IsoTop[OInfo, NInfo, ORepr, NRepr] {
  override def putDouble = rconv(l.putDouble)

  override def IOBind[A, B](implicit ai: NInfo[A], bi: NInfo[B]) = rconv(l.IOBind[A, B])

  override def IORet[A](implicit ai: NInfo[A]) = rconv(l.IORet[A])

  override def getDouble: NRepr[IO[Double]] = rconv(l.getDouble)

  override def IOInfo[A](implicit ai: NInfo[A]): NInfo[IO[A]] = iconv(l.IOInfo[A])

  override def IOElmInfo[A]: NInfo[IO[A]] => NInfo[A] = ioa => iconv(l.IOElmInfo(convi(ioa)))
}

object IsoIO {
  def apply[OInfo[_], NInfo[_], ORepr[_], NRepr[_]](implicit
                                                    ii: OInfo <~> NInfo,
                                                    ri: ORepr <~> NRepr,
                                                    lang: Lang[OInfo, ORepr]):
  IsoIO[OInfo, NInfo, ORepr, NRepr] =
    new IsoIO[OInfo, NInfo, ORepr, NRepr] {
      override val infoIso: OInfo <~> NInfo = ii

      override val reprIso: ORepr <~> NRepr = ri

      override val l: Lang[OInfo, ORepr] = lang
    }
}