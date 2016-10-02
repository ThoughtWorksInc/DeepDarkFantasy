package com.thoughtworks.DDF.Unit

import com.thoughtworks.DDF.RI.RILang

trait UnitLang[Info[_], Repr[_]] extends RILang[Info, Repr] {
  def mkUnit: Repr[Unit]

  implicit def UnitInfo: Info[Unit]
}
