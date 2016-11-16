package com.thoughtworks.DDF

import com.thoughtworks.DDF.Language.Lang

import scalaz.Isomorphism._

trait IsoBase[OInfo[_], NInfo[_], ORepr[_], NRepr[_]] {
  def infoIso: OInfo <~> NInfo

  def reprIso: ORepr <~> NRepr

  def l: Lang[OInfo, ORepr]

  def rconv[A]: ORepr[A] => NRepr[A] = reprIso.to.apply[A]

  def convr[A]: NRepr[A] => ORepr[A] = reprIso.from.apply[A]

  def iconv[A]: OInfo[A] => NInfo[A] = infoIso.to.apply[A]

  implicit def convi[A](implicit n: NInfo[A]): OInfo[A] = infoIso.from(n)
}
