package com.thoughtworks.DDF.Unit

trait UnitLang[Info[_], Repr[_]] {
  def mkUnit: Repr[Unit]

  def UnitInfo: Info[Unit]
}
