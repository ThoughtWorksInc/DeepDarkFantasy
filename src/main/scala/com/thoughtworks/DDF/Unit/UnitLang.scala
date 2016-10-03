package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.InfoB.InfoB

trait UnitLang[Info[_], Repr[_]] extends InfoB[Info, Repr] {
  def mkUnit: Repr[Unit]

  implicit def UnitInfo: Info[Unit]
}
